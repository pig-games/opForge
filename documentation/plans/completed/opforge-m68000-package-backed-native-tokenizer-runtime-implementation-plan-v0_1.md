# opForge Motorola 68000 Package-Backed Native Tokenizer Runtime Implementation Plan v0.1

## Metadata

- Source: `User instruction on 2026-04-14: implement this plan, follow the rules, starting with the smallest compliant slice`; the current native tokvm example in `examples/motorola68000/amigaos/tokvm/`; the tokenizer VM/runtime authority in `documentation/opforge-assembler-vm-path-guide-v0_1.md`; the native bring-up guidance in `documentation/libopforge-developer-guide.md`; the native ABI expectations in `documentation/vm-ultimate64-abi-contract-v1.md`; the current package/runtime contracts in `crates/opforge-package/src/package.rs` and `crates/opforge-vm/src/runtime_model_core.rs`; the current native envelope authority in `crates/opforge-vm/src/native6502_abi.rs` and `crates/opforge-vm/src/native6502.rs`; and the staged tokenizer rollout authority in `crates/opforge-vm/src/rollout.rs`
- Mode: `implementation`
- Owner: Codex

## Objective

Land a tokenizer-only native Motorola 68000 runtime that is driven by loaded
package content instead of the current embedded tokvm demo program.

This plan is intentionally narrower than a full native VM service. The target is
`load_package -> set_pipeline -> tokenize_line -> last_error` for the Motorola
68000 family, with module boundaries chosen so parser VM, encode-bytecode, and
expression work can be added later without restructuring the tokenizer path.

The current `tokvm/` AmigaOS example remains a separate tokenizer fixture path.
The package-backed runtime surface still lives under `tkpkg/`, but the shared
generic tokenizer interpreter may continue to live under `tokvm/` until a later
slice extracts it without collapsing the fixture/runtime split.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Scope is limited to a package-backed native tokenizer runtime for the
  Motorola 68000 family and the smallest AmigaOS-native surface needed to load a
  package, select a pipeline, tokenize one line, and report deterministic
  errors.
- Do not widen this plan into native parser VM, native expression VM, native
  encode-bytecode execution, native preprocessing, module-graph work, or a
  native full-assembler pass loop.
- Keep the current `examples/motorola68000/amigaos/tokvm/` path as a separate
  tokenizer fixture; do not grow it into the package-backed runtime.
- Package container parsing must follow the existing little-endian `.opasm`
  contract even though internal 68000-native working state may use big-endian
  in-memory fields where the local ABI requires it.
- The package-backed runtime must keep tokenizer semantics package-owned. Do not
  hard-code Motorola 68000 tokenizer behavior in interpreter control flow when a
  token policy field or tokenizer VM program already owns that behavior.
- The first native package-backed slice remains line-oriented and single-line
  only. Newline-containing input must fail deterministically until a later spec
  defines multi-line native iteration.
- The first native package-backed slice targets `.cpu 68020` as the baseline
  implementation CPU for AmigaOS examples unless a later spec changes that
  baseline.
- Reuse existing harness-fixture conventions under `examples/vm/harness-fixtures/`
  where practical instead of creating a parallel native-fixture taxonomy.
- The tokenizer-only Motorola 68000 runtime must lock its first native control
  block and entrypoint subset against the current v1 native envelope shape in
  `crates/opforge-vm/src/native6502_abi.rs` and `crates/opforge-vm/src/native6502.rs`:
  preserve the fixed control-block layout and width discipline, preserve the
  capability-bit model, preserve deterministic `status_code` and `last_error`
  behavior, and preserve the `init`, `load_package`, `set_pipeline`,
  `tokenize_line`, and `last_error` entrypoint subset while intentionally
  leaving `parse_line` and `encode_instruction` for later work.
- The first slice must also lock the byte-level request/response contract for
  that preserved subset against `crates/opforge-vm/src/native6502.rs`:
  `load_package` consumes raw package bytes, `set_pipeline` preserves the
  current NUL-delimited cpu/dialect payload contract, `tokenize_line`
  preserves the current line-number-plus-source-bytes payload shape,
  `last_error` preserves an empty request payload and deterministic UTF-8 error
  response semantics, and all payload lengths remain bounded by the current
  fixed-width native envelope.
- Any tokenizer parity claim in this plan is limited to tokenizer-path evidence
  backed by focused Rust/runtime corpus tests and the opt-in AmigaOS-native
  FS-UAE family-corpus gate added by this plan. This plan still does not claim
  parser, encoder, or full-assembler certification beyond those tokenizer-path
  checks.
- This plan must not become active until `plan-quality-reviewer` returns
  `PASS`.

## Planning Decisions Captured Up Front

- The new runtime lives in a new AmigaOS example directory separate from
  `tokvm/` so tokenizer-fixture work and package-runtime work do not collapse
  into one module tree.
- The tokenizer-only runtime should mirror the 6502 native harness boundary in
  shape, but only implement the tokenizer-relevant subset now: `init`,
  `load_package`, `set_pipeline`, `tokenize_line`, and `last_error`.
- The first work item must lock that tokenizer-only envelope explicitly against
  `crates/opforge-vm/src/native6502_abi.rs` and the request/response semantics
  in `crates/opforge-vm/src/native6502.rs` before package loading or tokenizer
  execution work begins.
- Parser, encode-bytecode, and expression work remain out of scope, but the
  module structure must leave obvious extension seams for them.
- The first package-backed tokenizer runtime must consume loaded token policy
  and tokenizer VM program data from the package rather than an embedded demo
  program.
- The first package-backed tokenizer runtime must support the full authoritative
  tokenizer opcode contract needed by package-owned programs, not just the
  explicit-scan subset used by the current tokvm fixture.
- Deterministic error namespace preservation (`OPC`, `OTR`, tokenizer
  diagnostics) is part of correctness, not just host-side polish.

## Planned Module Structure

- `examples/motorola68000/amigaos/tkpkg/tkpkg_entry.asm`: AmigaOS entry path and
  executable glue only.
- `examples/motorola68000/amigaos/tkpkg/tkpkg_abi.asm`: control-block layout,
  entry ordinals, capability flags, status codes, and shared offsets/constants.
- `examples/motorola68000/amigaos/tkpkg/tkpkg_service.asm`: request dispatch,
  lifecycle transitions, and top-level state machine.
- `examples/motorola68000/amigaos/tkpkg/tkpkg_package_loader.asm`: little-endian
  package header/TOC readers and owned tokenizer-related chunk loading.
- `examples/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm`: owner precedence
  resolution and active tokenizer policy/program selection.
- `examples/motorola68000/amigaos/tkpkg/tkpkg_token_policy.asm`: package-driven
  class, case, quote, comment, and operator helper logic.
- `examples/motorola68000/amigaos/tkpkg/tkpkg_tokenizer_vm.asm`: package-backed
  tokenizer VM bridge over loaded program bytes and package-selected policy,
  delegating to the shared generic interpreter entrypoints.
- `examples/motorola68000/amigaos/tkpkg/tkpkg_buffers.asm`: owned package
  storage, runtime state, active selection slots, token buffers, and last-error
  storage.
- `examples/motorola68000/amigaos/tkpkg/tkpkg_debug_cli.asm`: thin debug/smoke
  wrapper for `load_package -> set_pipeline -> tokenize_line -> last_error`
  without turning the core runtime modules into CLI code.

## Work Items

- [x] Work item 1: establish the new package-backed tokenizer runtime module tree and tokenizer-only native ABI boundary
  - Validation:
    - `cargo test -p asm motorola68020_tkpkg_native_abi_ -- --nocapture`
    - `cargo test -p asm motorola68020_tkpkg_native_abi_payloads_ -- --nocapture`
    - `cargo test -p asm motorola68020_tkpkg_native_wire_roundtrip_ -- --nocapture`
    - `cargo test -p asm motorola68020_tkpkg_module_surface_ -- --nocapture`
  - Definition of done:
    - a new `examples/motorola68000/amigaos/tkpkg/` module tree exists and is clearly separated from `tokvm/`
    - tokenizer-only entry ordinals, control-block offsets, capability bits, status semantics, and shared runtime constants live in one ABI module instead of being spread across implementation files
    - the first Motorola 68000 tokenizer envelope explicitly preserves the v1 native control-block layout, fixed-width field discipline, and deterministic `last_error` behavior from `crates/opforge-vm/src/native6502_abi.rs` and `crates/opforge-vm/src/native6502.rs`, while intentionally omitting `parse_line` and `encode_instruction`
    - the preserved byte-level request/response subset is explicitly locked: `load_package` raw-byte input, `set_pipeline` NUL-delimited cpu/dialect payloads, `tokenize_line` line-number-plus-source payloads, `last_error` empty request payloads and UTF-8 response semantics, and fixed-width payload length behavior
    - request dispatch and owned runtime storage have their own target modules from the start, while package loading, pipeline resolution, and tokenizer execution have dedicated module placeholders but no implemented semantics yet
  - Source requirement or finding IDs:
    - user request for a package-backed tokenizer plan with good module structure from the start
    - `documentation/libopforge-developer-guide.md` native bring-up order and module split guidance
    - `documentation/vm-ultimate64-abi-contract-v1.md` native envelope expectations
    - `crates/opforge-vm/src/native6502_abi.rs` v1 control-block and entrypoint subset authority
    - `crates/opforge-vm/src/native6502.rs` request lifecycle, `status_code`, and `last_error` authority
    - `crates/opforge-vm/src/native6502.rs` wire payload helpers and response round-trip authority
  - Expected files:
    - `examples/motorola68000/amigaos/tkpkg/tkpkg_entry.asm`
    - `examples/motorola68000/amigaos/tkpkg/tkpkg_abi.asm`
    - `examples/motorola68000/amigaos/tkpkg/tkpkg_service.asm`
    - `examples/motorola68000/amigaos/tkpkg/tkpkg_buffers.asm`
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to module scaffolding, tokenizer-only ABI and wire-contract locking, and focused assembly-surface validation with no package parsing or tokenizer semantics yet
  - Commit outcome:
    - the repo has a dedicated structural home for a package-backed Motorola 68000 tokenizer runtime that does not depend on the existing tokvm fixture layout, and its tokenizer-only ABI and wire-contract subset is locked before later slices depend on it

- [x] Work item 2: implement tokenizer-relevant package loading and owned runtime state
  - Validation:
    - `cargo test -p asm motorola68020_tkpkg_load_package_ -- --nocapture`
    - `cargo test -p asm motorola68020_tkpkg_error_namespace_ -- --nocapture`
  - Definition of done:
    - the native runtime can validate and ingest `.opasm` package bytes using little-endian container rules
    - tokenizer-related package data needed by this plan is copied into native-owned state rather than borrowed from caller buffers
    - malformed package bytes fail deterministically through the native error surface instead of falling into undefined interpreter behavior
  - Source requirement or finding IDs:
    - `documentation/vm-ultimate64-abi-contract-v1.md` package ownership and little-endian contract
    - `crates/opforge-package/src/package.rs` tokenizer package descriptors
    - `crates/opforge-vm/src/native6502.rs` load-package lifecycle reference
  - Expected files:
    - `examples/motorola68000/amigaos/tkpkg/tkpkg_package_loader.asm`
    - `examples/motorola68000/amigaos/tkpkg/tkpkg_buffers.asm`
    - `examples/vm/harness-fixtures/*` if shared failure fixtures need extension
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`
    - `cargo test -p vm execution_model_tokenizer_ -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to package ingestion, owned-state setup, and deterministic tokenizer-surface package failures
  - Commit outcome:
    - the native runtime can load tokenizer-relevant package content and retain it independently of caller-owned package buffers

- [x] Work item 3: add package-backed pipeline resolution and active tokenizer selection
  - Validation:
    - `cargo test -p asm motorola68020_tkpkg_set_pipeline_ -- --nocapture`
    - `cargo test -p asm motorola68020_tkpkg_owner_precedence_ -- --nocapture`
  - Definition of done:
    - the native runtime can resolve the active Motorola 68000 family tokenizer program and token policy for a selected cpu/dialect pipeline
    - owner precedence is deterministic and matches the package-driven Rust runtime model for the tokenizer-relevant scopes used in this plan
    - tokenizer execution no longer relies on embedded demo bytecode or hardcoded token policy tables
  - Source requirement or finding IDs:
    - `crates/opforge-vm/src/runtime_model_core.rs` pipeline and tokenizer-program resolution behavior
    - `crates/opforge-vm/src/builder.rs` default family token policy and tokenizer VM descriptors
    - user requirement to keep module structure extensible from the start
  - Expected files:
    - `examples/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm`
    - `examples/motorola68000/amigaos/tkpkg/tkpkg_token_policy.asm`
    - `examples/motorola68000/amigaos/tkpkg/tkpkg_buffers.asm`
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`
    - `cargo test -p vm execution_model_tokenizer_ -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to package-backed tokenizer selection and owner-precedence resolution with no parser or encoder work
  - Commit outcome:
    - the active tokenizer policy and VM program are selected from the loaded package instead of being baked into assembly constants

- [x] Work item 4: implement the generic native tokenizer VM executor over loaded package programs
  - Validation:
    - `cargo test -p asm motorola68020_tkpkg_tokenize_line_ -- --nocapture`
    - `cargo test -p asm motorola68020_tkpkg_tokenizer_parity_ -- --nocapture`
    - `cargo test -p vm motorola68000_tokenizer_vm_staged_baseline_ -- --nocapture`
  - Definition of done:
    - the package-backed native tokenizer execution path supports the authoritative tokenizer opcode surface needed by package-backed programs for this plan, including lexeme-building and state-table behavior
    - tokenizer compatibility checks, budgets, diagnostics, and deterministic failure behavior match the tokenizer-side runtime invariants already enforced by Rust
    - focused staged Motorola 68000 corpus parity coverage exists in Rust/runtime tests for the opcode and policy cases exercised by this slice, and those checks remain the rollout authority for staged host-side tokenizer parity
    - package-driven token policy fields control character-class and token-shaping behavior instead of interpreter-local Motorola 68000 branches
    - the `tkpkg/` runtime remains the package-backed service surface even if the shared generic interpreter implementation continues to live under `tokvm/`
  - Source requirement or finding IDs:
    - `crates/opforge-package/src/package.rs` tokenizer opcode contract
    - `crates/opforge-vm/src/runtime_model_core.rs` tokenizer compatibility and execution invariants
    - `crates/opforge-vm/src/tokenizer_runtime_utils.rs` policy-driven helper behavior
    - `crates/opforge-vm/src/rollout.rs` staged Motorola 68000 tokenizer rollout authority
  - Expected files:
    - `examples/motorola68000/amigaos/tkpkg/tkpkg_tokenizer_vm.asm`
    - `examples/motorola68000/amigaos/tkpkg/tkpkg_token_policy.asm`
    - `examples/motorola68000/amigaos/tkpkg/tkpkg_service.asm`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-vm/src/rollout.rs`
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`
    - `cargo test -p vm execution_model_tokenizer_ -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to tokenizer opcode execution, staged Motorola 68000 baseline parity locking, and deterministic diagnostic behavior with no parser or encoder expansion
  - Commit outcome:
    - a package-backed native Motorola 68000 tokenizer runtime can execute loaded tokenizer VM programs over one newline-free line and return deterministic tokenizer results or failures, with focused staged Rust corpus parity evidence for the cases implemented in this slice

- [x] Work item 5: add the AmigaOS-native smoke wrapper and reference-backed smoke validation phase
  - Validation:
    - `cargo test -p asm motorola68020_tkpkg_smoke_ -- --nocapture`
    - `cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - `cargo test --manifest-path crates/opforge-asm/Cargo.toml external_fs_uae_tkpkg_native_motorola68000_family_corpus_matches_vm_authoritative_rows -- --nocapture`
  - Definition of done:
    - Phase 5a for this commit is a thin AmigaOS executable path that established `init -> load_package -> set_pipeline -> last_error` against the package-backed tokenizer runtime without folding CLI/report code into the core tokenizer modules
    - after Work item 4 lands, that same wrapper extends to a focused `tokenize_line` smoke over a fixed sample line while keeping CLI/report code out of the core tokenizer modules
    - focused host-side tests and reference artifacts lock the assembly surface, smoke flow, and deterministic failure paths for the new runtime
    - the debug CLI smoke path is CPU-selectable across the checked Motorola 68000 family targets and has an opt-in FS-UAE parity gate that matches Rust VM-authoritative debug rows across the current top-level `examples/motorola68000/*.asm` corpus
    - the existing `tokvm/` example remains separate and continues to validate its own narrower tokenizer-fixture contract
  - Source requirement or finding IDs:
    - `documentation/libopforge-developer-guide.md` native bring-up order
    - `documentation/vm-ultimate64-abi-contract-v1.md` native smoke-flow expectations
    - existing native fixture/report conventions under `examples/vm/harness-fixtures/`
  - Expected files:
    - `examples/motorola68000/amigaos/tkpkg/tkpkg_debug_cli.asm`
    - `examples/reference/motorola68000/amigaos/tkpkg_*.hunk`
    - `examples/reference/motorola68000/amigaos/tkpkg_*.lst`
    - `crates/opforge-asm/src/tests.rs`
    - `examples/vm/harness-fixtures/*` if shared smoke fixtures need tokenizer-runtime additions
  - Full quality gates:
    - `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`
    - `cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the tokenizer-only native smoke wrapper, host-side locking tests, and reference artifacts
  - Commit outcome:
    - the package-backed AmigaOS debug wrapper now supports opt-in native tokenizer parity sweeps across the current top-level Motorola 68000 family corpus while keeping CLI/report code out of the core runtime modules
    - the repo has a package-backed Motorola 68000 tokenizer runtime with a stable AmigaOS smoke path and reference-backed validation, ready to serve as the tokenizer foundation for later native parser and encoder work

## Milestones

- [x] Milestone 1: the new runtime boundary and module structure exist independently of `tokvm/` (`Work item 1`)
- [x] Milestone 2: tokenizer-relevant package data can be loaded and owned natively (`Work item 2`)
- [x] Milestone 3: active tokenizer selection is package-backed and pipeline-resolved (`Work item 3`)
- [x] Milestone 4: the native tokenizer executes loaded VM programs with deterministic parity and diagnostics (`Work item 4`)
- [x] Milestone 5: the new runtime has a thin AmigaOS smoke wrapper and reference-backed validation (`Work item 5`)

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping

## To Be Planned Later

- native parser VM execution for Motorola 68000 family pipelines
- native encode-bytecode execution and mode-selector resolution
- native expression evaluation and expression-parser compatibility behavior
- a full Motorola 68000 package service surface that reaches `parse_line` and `encode_instruction`
- multi-line native tokenization and refillable stream behavior
- any attempt to replace the current Rust outer orchestration with a native full-assembler loop

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no plan activation before `plan-quality-reviewer` returns `PASS`
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- do not collapse package-backed service, package-loading, pipeline, or buffer modules into `examples/motorola68000/amigaos/tokvm/`; keep the fixture/runtime split intact even if the shared generic interpreter implementation remains there temporarily
- do not add parser, expression, or encoder logic into tokenizer modules as a shortcut for “future-proofing”; keep those seams explicit but unimplemented
- do not hard-code Motorola 68000 tokenizer semantics in executor control flow when the loaded package policy or tokenizer program is supposed to own them
- do not bypass little-endian package decoding rules because the host CPU is big-endian