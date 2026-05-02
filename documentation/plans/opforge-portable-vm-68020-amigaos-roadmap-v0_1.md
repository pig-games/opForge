# opForge Portable VM to 68020 AmigaOS Roadmap v0.1

## Metadata

- Source: User request on 2026-05-02 to write down the updated next steps list
  and expand it into a more detailed roadmap for reaching enough VM
  implementation to port to 68020 AmigaOS as fast as possible. Updated the same
  day to incorporate the separate native AmigaOS module/use specification and
  implementation plan, then updated again to add early `.include` support as a
  host-owned preprocessor/input-expansion track.
- Mode: migration
- Owner: opForge implementation agent

## Objective

Define the fastest practical route from the current VM state to a portable
68020 AmigaOS assembler slice that can tokenize, recognize host-owned project
bootstrap directives, parse opasm statements through VM contracts, parse the
needed mathematical expression ranges through `EXVM`, and stop at a clear
native emitter boundary.

The target is not a full native assembler in one jump. The target is the
smallest end-to-end AmigaOS-native pipeline that proves the VM contracts are
portable and useful:

1. read a source file through the native CLI harness;
2. keep compact `.module`, `.use`, and `.include` bootstrap/input expansion
  host-owned;
3. tokenize each expanded source line through native `TKVM` package execution;
4. parse delegated opasm statements through native `PRVM` package execution;
5. parse mathematical operand expression ranges through native `EXVM` package
   execution;
6. emit a deterministic intermediate report or emitter-stub boundary that can
   drive the next native implementation pass.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during roadmap execution.
- This roadmap must not become active until `plan-quality-reviewer` or
  `plan-quality-orchestrator` returns `PASS`.
- One active work item at a time.
- Each work item or phase must end in exactly one new commit before the next
  item begins.
- Full quality gates are mandatory before each commit.
- `plan-compliance-reviewer` must pass before each plan-driven commit.
- Native AmigaOS implementation slices target `.cpu 68020` first while keeping
  existing `*_run_68000` symbol naming where already reserved by ABI specs.
- Do not move module graph orchestration, include expansion, macro expansion,
  symbol resolution, instruction selection, instruction encoding, relocation,
  Hunk writing, or full assembler pass control into `TKVM`, `PRVM`, or `EXVM`.
- `.include` is host-owned preprocessor/input expansion. It may feed `TKVM`,
  `PRVM`, and later macro/directive stages, but it is not itself a VM contract.
  The first native slice should support the common Amiga source shape while
  preserving a structured path for later preprocessor functionality such as
  conditional inclusion, define tables, file-once guards, and macro-source
  expansion.
- Bootstrap `.module` and `.use` discovery remains host-owned and compact. It
  may be hard-coded in Rust or native CLI host code, but it must stay narrow and
  must not become a second pass-time parser.
- Native `.module`, `.endmodule`, `.use`, and `--module-path` behavior is
  governed by `documentation/opForge-native-amigaos-module-use-port-spec-v0_1.md`
  and `documentation/plans/opforge-native-amigaos-module-use-port-implementation-plan-v0_1.md`.
  This roadmap may depend on that track, but it must not duplicate or silently
  override its decisions.
- The module/use track preserves the Rust split between parser surfaces, source
  graph loading, and macro export injection. This roadmap must not collapse that
  split while wiring the VM front end.
- Selected-item aliases in `.use` are parser-record parity data for now: Rust
  parses them, but current source-graph import injection carries selected item
  names and does not yet apply selected-item aliases semantically. Native work
  must keep that distinction explicit.
- `PRVM` owns opasm statement and operand-shape parsing only after opcore or the
  native host has classified a line as delegated opasm work.
- `EXVM` owns operand-shape-free mathematical expression token ranges only.
  CPU-family operand wrappers remain in `PRVM`/opasm handling.
- `EXVM` and `PRVM` bytecode must remain separate contracts. Cross-contract
  calls use typed request/result records or direct native ABI calls, not embedded
  bytecode from another contract.
- Existing package byte ranges and reservation discipline remain binding for new
  opcodes. Do not consume reserved opcode space without updating the relevant
  contract plan or spec first.
- FS-UAE evidence is opt-in unless a work item explicitly states otherwise.
  Default validation must stay runnable without an emulator.
- Fixture/reference regeneration is allowed only when an expected artifact or
  report changes by design. Never update fixtures to hide an unexpected failure.

## Current Baseline

- `TKVM` has a native AmigaOS 68020 path and an `opforge` native CLI harness
  that can drive tokenizer package execution over source lines.
- `PRVM` v2 is authoritative in Rust for opasm statement parsing, with a native
  `prvm_run_68000` ABI and first native smoke slices already planned and partly
  implemented in earlier work.
- `EXVM` is authoritative in Rust for covered mathematical expression parsing:
  literals, symbols, grouping, unary and binary operators, ternaries, ranges,
  lists, structs, member access, and indexing. Calls and placeholders are
  explicit compatibility/out-of-scope nodes.
- `PRVM` routes covered operand expression token ranges through `EXVM` in the
  Rust runtime path while preserving CPU-family operand wrappers outside `EXVM`.
- The canonical boundary spec says module graph dependency traversal is
  host-owned and does not use a VM scanner.
- `.include` support has not landed in the native AmigaOS CLI yet. It is needed
  early because many Amiga sources use include files for constants, macros, and
  structure definitions before the first instruction line.
- The native AmigaOS `opforge` CLI now has transitional hard-coded `.module`
  and `.use` visibility scanning. This roadmap treats that as the first
  host-owned bootstrap slice and does not rewrite it.
- `documentation/opForge-native-amigaos-module-use-port-spec-v0_1.md` now
  defines the native module/use port contract for `.module`, `.endmodule`,
  `.use`, repeatable `-M` / `--module-path`, fixed-capacity module/import/path
  tables, deterministic diagnostics, and the selected-item alias limitation.
- `documentation/plans/opforge-native-amigaos-module-use-port-implementation-plan-v0_1.md`
  decomposes that contract into commit-sized native AmigaOS implementation
  slices. Its next concrete step after the transitional tokenizer-post
  visibility scanner is fixed-capacity native module/use state tables.

## Fastest-Path Strategy

The fastest route is to avoid building a full opcore directive VM, native macro
system, or native emitter before the VM portability question is answered.
Instead, keep project/bootstrap work host-owned, wire the already-proven native
tokenizer and parser components into one line-oriented CLI path, and add only
the `EXVM` subset needed to remove Rust expression-parser dependency from the
first AmigaOS operand scenarios. Add `.include` before the parser VM integration
so the front end sees the source stream that real Amiga projects commonly use,
but keep it as a small input-expansion/preprocessor layer rather than a broad
native macro system.

The first useful native target should be a report-producing assembler-front-end
slice, not a linkable object writer. A successful run should prove that AmigaOS
can execute package-backed `TKVM`, `PRVM`, and `EXVM` contracts over real source
text and reach an explicit emitter boundary with deterministic records.

## Companion Module/Use Track

The module/use artifacts are the authoritative host-bootstrap track for this
roadmap:

- `documentation/opForge-native-amigaos-module-use-port-spec-v0_1.md`
- `documentation/plans/opforge-native-amigaos-module-use-port-implementation-plan-v0_1.md`

This roadmap consumes that track as a prerequisite for native project/bootstrap
state. It does not replace that plan's item ordering. The fastest VM-port route
is now:

1. land the module/use plan's transitional tokenizer-post visibility scanner,
  followed by fixed-capacity state tables and table-backed `.module` / `.use`
  minimum;
2. keep module resolution and macro export injection deferred until their
  smallest native slices are needed;
3. wire `PRVM` and `EXVM` around the resulting host-owned bootstrap state.

Before Work item 5 in this roadmap starts, the module/use companion plan must
have either completed enough work to provide stable table-backed `.module` and
`.use` records, or this roadmap must be updated again with the exact narrower
bootstrap substitute being used for the first VM pipeline smoke. The
transitional visibility scanner is only enough to unblock early `.include`
input-expansion work; it is not enough to satisfy the table-backed bootstrap
state needed by parser VM CLI integration.

## Include / Preprocessor Track

`.include` is the first explicit preprocessor/input-expansion feature in this
roadmap. It should land early because it is common in Amiga assembly sources,
but it must be shaped as an input stream service that later preprocessor work can
extend.

The initial native `.include` slice should:

1. recognize only a narrow Rust-compatible `.include "path"` form, plus any
  already-supported quote/path spelling verified from the Rust parser before
  implementation;
2. maintain a fixed-capacity include stack with parent file, child file, source
  line, and expansion depth records;
3. emit expanded source lines with stable logical source-location records so
  tokenizer, parser, diagnostics, and future emitter work can report the
  original file and line;
4. reject missing files, recursive include depth overflow, path length overflow,
  malformed include directives, and table overflow deterministically;
5. leave macro expansion, conditional assembly, include-once semantics, and
  define substitution explicitly deferred but represented by extension points in
  the include/preprocessor state model.

This track is independent from module graph loading. `.include` expands source
text for the current compilation stream; `.use` loads module dependencies and
import records. They may share path-root lookup helpers later, but the records
and diagnostics should remain distinct.

## Work Items

- [x] Work item 1: adopt the native module/use companion track as the bootstrap prerequisite
  - Source requirement or finding IDs: user decision that module bootstrap
    scanning remains hard-coded for now; `documentation/opForge-native-amigaos-module-use-port-spec-v0_1.md`; `documentation/plans/opforge-native-amigaos-module-use-port-implementation-plan-v0_1.md`.
  - Validation: See Full quality gates for this work item.
  - Definition of done: See detailed criteria below for this work item.
  - Expected files:
    - `documentation/opForge-native-amigaos-module-use-port-spec-v0_1.md`
    - `documentation/plans/opforge-native-amigaos-module-use-port-implementation-plan-v0_1.md`
    - this roadmap for progress evidence
  - Implementation slice:
    - Treat the module/use spec and implementation plan as the authoritative
      source for native bootstrap work instead of duplicating their content here.
    - Verify that their artifact checks pass and that the implementation plan is
      execution-safe under the active `AGENTS.md` rules.
    - If another agent is still actively editing the native CLI implementation
      files, do not adopt those changes without explicit user approval.
    - Record in this roadmap which module/use implementation-plan items must be
      complete before Work item 5 can begin. The expected minimum is table-backed
      `.module` and `.use` records; `--module-path`, external module resolution,
      and PRVM-fed module/use records may remain deferred unless a later slice
      needs them.
  - Full quality gates:
    - `python3 scripts/workflow/check_spec_artifact.py documentation/opForge-native-amigaos-module-use-port-spec-v0_1.md`
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-native-amigaos-module-use-port-implementation-plan-v0_1.md`
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-portable-vm-68020-amigaos-roadmap-v0_1.md`
    - `cargo fmt --all --check`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit --no-fetch`
    - `cargo test --workspace`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with this
    roadmap, the companion-artifact incorporation summary, changed files, and
    validation logs before committing.
  - Commit outcome: One commit that links this roadmap to the module/use
    companion spec and plan without touching native implementation behavior.
  - Definition of done:
    - The module/use spec and implementation plan are named as companion
      authorities for bootstrap work.
    - The roadmap records the minimum module/use state needed before `PRVM` CLI
      integration starts.
    - Selected-item alias semantics and `.use ... with(...)` deferral are not
      lost in the broader VM roadmap.
    - No native CLI source, tests, or reference artifacts are changed by this
      incorporation item.

- [x] Work item 2: define native `.include` and preprocessor input-expansion records
  - Source requirement or finding IDs: user request to add `.include` support
    early because it is common in Amiga sources, while structuring the work for
    later preprocessor expansion.
  - Validation: See Full quality gates for this work item.
  - Definition of done: See detailed criteria below for this work item.
  - Expected files:
    - `documentation/opForge-native-include-preprocessor-input-spec-v0_1.md` (new)
    - `documentation/vm-boundary-protocol-v1.md` if a brief boundary
      cross-reference is needed
    - this roadmap for progress evidence only
  - Implementation slice:
    - Specify the native `.include` directive subset and its relationship to
      tokenizer input, module/use graph handling, and future preprocessor work.
    - Define fixed-capacity include stack, include path, expanded-line, and
      logical source-location records.
    - Define diagnostics for missing include files, malformed include syntax,
      recursive depth overflow, path length overflow, and table overflow.
    - Name the extension points for future preprocessor features without
      implementing them in the first `.include` slice.
  - Full quality gates:
    - `python3 scripts/workflow/check_spec_artifact.py documentation/opForge-native-include-preprocessor-input-spec-v0_1.md`
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-portable-vm-68020-amigaos-roadmap-v0_1.md`
    - `cargo fmt --all --check`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit --no-fetch`
    - `cargo test --workspace`
    - `cargo test --locked`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with this
    roadmap, the include/preprocessor input spec, changed files, and validation
    logs.
  - Commit outcome: One commit creating the `.include` / preprocessor input
    contract and updating roadmap bookkeeping only.
  - Definition of done:
    - `.include` is clearly modeled as host-owned input expansion rather than a
      `TKVM`, `PRVM`, or `EXVM` responsibility.
    - The first supported syntax and all deferred preprocessor features are
      explicit.
    - Later conditional assembly, define substitution, macro-source expansion,
      and include-once behavior have named state-model extension points.

- [x] Work item 3: land first native `.include` expansion in the CLI input stream
  - Source requirement or finding IDs: Work item 2 include/preprocessor input
    contract; common Amiga source requirement for early include-file support.
  - Validation: See Full quality gates for this work item.
  - Definition of done: See detailed criteria below for this work item.
  - Expected files:
    - `examples/motorola68000/amigaos/opforge/opforge_cli.asm`
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-asm/src/fs_uae_smoke.rs`
    - reference Hunk/listing artifacts for changed native CLI output
  - Implementation slice:
    - Add fixed-capacity native include state and path buffers.
    - Recognize the first supported `.include` directive in the CLI input stream
      after tokenizer success for the directive line and before delegated opasm
      statement parsing.
    - Open the included file, feed its lines through the same tokenizer and
      bootstrap/preprocessor path, and restore the parent input stream when the
      include returns.
    - Emit deterministic include report records and logical source locations for
      expanded lines.
    - Keep macro expansion, conditional assembly, include-once semantics, and
      define substitution deferred.
  - Full quality gates:
    - `cargo test -p asm motorola68020_opforge_native_cli -- --nocapture`
    - `cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - `cargo test -p asm external_fs_uae_opforge_native_cli_reports_module_use_parser_status -- --nocapture` with clean skip when not configured
    - `cargo fmt --all --check`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit --no-fetch`
    - `cargo test --workspace`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` before the
    commit with evidence that this slice adds only host-owned `.include` input
    expansion and does not start macro or conditional assembly.
  - Commit outcome: One commit that makes the native CLI expand one included
    file path into the tokenizer/parser input stream.
  - Definition of done:
    - A smoke source can include one file and process the included lines through
      the existing native tokenizer path.
    - Missing include, malformed include, depth overflow, and path/table overflow
      cases report deterministic native diagnostics.
    - Logical file/line information survives expansion well enough for report
      records and focused tests.
    - The implementation leaves clear state hooks for later preprocessor
      features without adding them prematurely.

- [ ] Work item 4: define the native VM pipeline report and handoff records
  - Source requirement or finding IDs: need a durable intermediate boundary
    before native emission; current tokenizer and parser native report patterns;
    companion module/use spec record and diagnostic requirements; Work item 2
    include/preprocessor records.
  - Validation: See Full quality gates for this work item.
  - Definition of done: See detailed criteria below for this work item.
  - Expected files:
    - `documentation/opForge-native-vm-pipeline-report-v0_1.md` (new)
    - `documentation/vm-boundary-protocol-v1.md` if a brief cross-reference is
      needed
    - this roadmap for progress evidence only
  - Implementation slice:
    - Specify the `OPFORGE-NATIVE 1` report records produced by the native CLI
      as it advances from bootstrap to tokenizer to parser to expression stages.
    - Include module/use table summary records produced by the companion track,
      including module ids, import module ids, import aliases, selected item
      names, and an explicit marker for parser-record-only selected-item aliases.
    - Include `.include` and preprocessor input-expansion records produced by
      Work item 3, including parent file, child file, depth, and logical source
      location mapping.
    - Define compact statement-result, expression-result, diagnostic, and
      emitter-boundary records that are easy to write from 68020 assembly and
      easy to decode in Rust tests.
    - State that the records are a temporary front-end report contract until a
      native emitter plan replaces the stub.
  - Full quality gates:
    - `python3 scripts/workflow/check_spec_artifact.py documentation/opForge-native-vm-pipeline-report-v0_1.md`
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-portable-vm-68020-amigaos-roadmap-v0_1.md`
    - `cargo fmt --all --check`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit --no-fetch`
    - `cargo test --workspace`
    - `cargo test --locked`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with this
    roadmap, the report-spec slice, changed files, and validation logs.
  - Commit outcome: One commit with the report/handoff spec and roadmap
    bookkeeping only.
  - Definition of done:
    - The report names each stage and status code used by later native slices.
    - Statement and expression record layouts are fixed-width or otherwise
      bounded for 68020-friendly writes.
    - The spec explicitly keeps emitter/object generation out of this roadmap.

- [ ] Work item 5: integrate native `PRVM` into the `opforge` CLI for a no-expression statement
  - Source requirement or finding IDs: existing `prvm_run_68000` native ABI and
    smoke path; fastest route to package-backed native statement parsing.
  - Validation: See Full quality gates for this work item.
  - Definition of done: See detailed criteria below for this work item.
  - Expected files:
    - `examples/motorola68000/amigaos/opforge/opforge_cli.asm`
    - `examples/motorola68000/amigaos/prvm/prvm_interpreter.asm`
    - `crates/opforge-asm/src/tests.rs`
    - `examples/reference/motorola68000/amigaos/opforge/opforge_cli.hunk`
    - `examples/reference/motorola68000/amigaos/opforge/opforge_cli.lst`
  - Implementation slice:
    - Before editing native CLI sources, confirm the module/use companion plan
      has landed its table-backed `.module` and `.use` minimum, or update this
      roadmap with the approved substitute bootstrap state.
    - Confirm the first native `.include` expansion slice has landed, or update
      this roadmap with the approved reason for running the first PRVM smoke on
      a single-file source stream.
    - Link or include the existing native `PRVM` interpreter in the native CLI
      executable.
    - Feed tokenizer output for exactly one no-expression opasm statement shape
      into `prvm_run_68000`.
    - Report decoded native statement records through the Work item 4 report
      contract and keep the emitter stub as the terminal stage.
  - Full quality gates:
    - `cargo test -p asm motorola68020_prvm -- --nocapture`
    - `cargo test -p asm motorola68020_opforge_native_cli -- --nocapture`
    - `cargo test -p vm native_prvm_abi -- --nocapture`
    - `cargo fmt --all --check`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit --no-fetch`
    - `cargo test --workspace`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` before the
    commit with evidence that this slice integrates `PRVM` only for the
    no-expression statement path.
  - Commit outcome: One commit that makes the native CLI execute tokenizer plus
    parser VM for one no-expression opasm line.
  - Definition of done:
    - The CLI still handles `.module`/`.use` through the companion host-owned
      bootstrap state.
    - A simple delegated line such as an indented `NOP` reaches `PRVM` and emits
      deterministic statement records.
    - Expression-request statuses are still reported as unsupported/deferred.
    - No emitter, symbol resolver, or instruction encoder is added.

- [ ] Work item 6: broaden native `PRVM` to expression-requesting operands
  - Source requirement or finding IDs: Rust PRVM v2 operand-boundary contract;
    native PRVM ABI expression-request records.
  - Validation: See Full quality gates for this work item.
  - Definition of done: See detailed criteria below for this work item.
  - Expected files:
    - `examples/motorola68000/amigaos/prvm/prvm_interpreter.asm`
    - `examples/motorola68000/amigaos/opforge/opforge_cli.asm`
    - `crates/opforge-vm/tests/parser_vm_native_abi.rs`
    - `crates/opforge-asm/src/tests.rs`
  - Implementation slice:
    - Extend native `PRVM` only far enough to parse one m68k operand statement
      that requires an expression sub-call, for example `MOVE.W 1+2(A0),D0`.
    - Emit an expression-request record containing the exact operand token range
      and continuation slot.
    - Keep the continuation unresolved until native `EXVM` lands in Work item 8.
  - Full quality gates:
    - `cargo test -p vm native_prvm_abi -- --nocapture`
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture`
    - `cargo test -p asm motorola68020_prvm -- --nocapture`
    - `cargo fmt --all --check`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit --no-fetch`
    - `cargo test --workspace`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` before the
    commit with evidence that the slice adds only expression-request production,
    not expression parsing.
  - Commit outcome: One commit that proves native `PRVM` can stop at the same
    expression boundary used by Rust `PRVM`.
  - Definition of done:
    - Native `PRVM` preserves m68k operand wrappers outside the expression range.
    - The expression-request token range matches the Rust PRVM boundary for the
      focused fixture.
    - The CLI reports the deferred expression request deterministically.

- [ ] Work item 7: define the native `EXVM` single-range ABI
  - Source requirement or finding IDs: completed Rust `EXVM` contract; PRVM
    expression-request records from Work item 6; stale native symbol notes that
    predate the `EXVM` rename.
  - Validation: See Full quality gates for this work item.
  - Definition of done: See detailed criteria below for this work item.
  - Expected files:
    - `documentation/opForge-m68000-expression-vm-single-range-abi-spec-v0_1.md` (new)
    - `documentation/opforge-assembler-vm-path-guide-v0_1.md`
    - `documentation/plans/opforge-parser-vm-v2-opasm-statement-implementation-plan-v0_1.md` only if stale `expvm_run_68000` naming must be corrected
    - this roadmap for progress evidence only
  - Implementation slice:
    - Define `exvm_run_68000` as the native expression parser VM entrypoint for
      one operand-shape-free token range.
    - Define caller-owned token, lexeme, output-expression, diagnostic, and
      budget fields.
    - State how `PRVM` resumes after a successful expression result or terminal
      expression diagnostic.
    - Correct stale native expression symbol naming from `expvm` to `exvm` where
      it is still normative rather than historical.
  - Full quality gates:
    - `python3 scripts/workflow/check_spec_artifact.py documentation/opForge-m68000-expression-vm-single-range-abi-spec-v0_1.md`
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-portable-vm-68020-amigaos-roadmap-v0_1.md`
    - `cargo fmt --all --check`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit --no-fetch`
    - `cargo test --workspace`
    - `cargo test --locked`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` before the
    commit with evidence that this slice is spec-only plus naming cleanup.
  - Commit outcome: One commit that creates the native `EXVM` ABI authority.
  - Definition of done:
    - The native entrypoint name, input token range, output record, diagnostics,
      budgets, and PRVM resume behavior are unambiguous.
    - The spec keeps calls/placeholders out of the first native `EXVM` slice.
    - No native `EXVM` assembly is landed in this item.

- [ ] Work item 8: land the first native `EXVM` scalar/arithmetic interpreter slice
  - Source requirement or finding IDs: Work item 7 ABI; completed Rust `EXVM`
    scalar and arithmetic coverage.
  - Validation: See Full quality gates for this work item.
  - Definition of done: See detailed criteria below for this work item.
  - Expected files:
    - `examples/motorola68000/amigaos/exvm/exvm_interpreter.asm` (new)
    - `examples/reference/motorola68000/amigaos/exvm_interpreter.hunk`
    - `examples/reference/motorola68000/amigaos/exvm_interpreter.lst`
    - `crates/opforge-vm/tests/exvm_native_abi.rs` or the nearest focused native
      EXVM test surface
    - `crates/opforge-asm/src/tests.rs`
  - Implementation slice:
    - Implement `exvm_run_68000` for literals, identifiers, grouping, unary
      operators, and core binary arithmetic needed by the first operand fixture.
    - Return compact expression records compatible with Work item 7.
    - Reject unsupported covered or out-of-scope nodes deterministically.
  - Full quality gates:
    - `cargo test -p vm exvm -- --nocapture`
    - `cargo test -p vm native_exvm_abi -- --nocapture` or the nearest focused
      native EXVM ABI test filter
    - `cargo test -p asm motorola68020_exvm -- --nocapture`
    - `cargo fmt --all --check`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit --no-fetch`
    - `cargo test --workspace`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` before the
    commit with evidence that this slice implements only the first native scalar
    arithmetic path.
  - Commit outcome: One commit that makes `EXVM` executable natively for the
    first operand-expression scenario.
  - Definition of done:
    - Native `EXVM` can parse the focused expression range without Rust fallback.
    - Invalid opcodes, budget exhaustion, stack overflow, and unsupported syntax
      return deterministic diagnostics.
    - The implementation consumes package-provided `EXVM` bytecode rather than
      hard-coding one source expression in assembly.

- [ ] Work item 9: resume native `PRVM` from native `EXVM` results inside the CLI
  - Source requirement or finding IDs: Work items 6 and 8; Rust PRVM-to-EXVM
    expression boundary.
  - Validation: See Full quality gates for this work item.
  - Definition of done: See detailed criteria below for this work item.
  - Expected files:
    - `examples/motorola68000/amigaos/opforge/opforge_cli.asm`
    - `examples/motorola68000/amigaos/prvm/prvm_interpreter.asm`
    - `examples/motorola68000/amigaos/exvm/exvm_interpreter.asm`
    - `crates/opforge-asm/src/tests.rs`
    - reference Hunk/listing artifacts for changed examples
  - Implementation slice:
    - When native `PRVM` reports an expression request, call `exvm_run_68000`
      over the requested token range and write the result slot back into the
      native `PRVM` resume buffer.
    - Resume `PRVM` and emit a complete statement report for the focused m68k
      operand fixture.
  - Full quality gates:
    - `cargo test -p asm motorola68020_opforge_native_cli -- --nocapture`
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture`
    - `cargo test -p vm exvm -- --nocapture`
    - `cargo fmt --all --check`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit --no-fetch`
    - `cargo test --workspace`
    - Opt-in FS-UAE native CLI smoke when configured
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` before the
    commit with evidence that this slice only wires `PRVM` to `EXVM` and still
    stops before emission.
  - Commit outcome: One commit that proves native `TKVM` plus native `PRVM` plus
    native `EXVM` can process one realistic m68k line in the CLI.
  - Definition of done:
    - The focused fixture reaches tokenizer, parser, expression parser, and
      emitter-stub stages in order.
    - The parsed statement/operand report matches Rust-normalized expectations
      for the covered path.
    - Expression failures propagate through the CLI as deterministic diagnostics.

- [ ] Work item 10: widen the native CLI smoke corpus to the smallest useful AmigaOS set
  - Source requirement or finding IDs: need enough coverage to iterate on native
    AmigaOS assembler behavior after the first VM pipeline works.
  - Validation: See Full quality gates for this work item.
  - Definition of done: See detailed criteria below for this work item.
  - Expected files:
    - `crates/opforge-asm/src/fs_uae_smoke.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `examples/motorola68000/amigaos/opforge/opforge_cli.asm`
    - reference artifacts only when intentionally changed
  - Implementation slice:
    - Add a tiny multi-line smoke source with `.module`, `.use`, label plus
      `.include`, instruction text from the included file, no-expression
      instruction, expression operand instruction, `.endmodule`, and a
      comment/blank-line case.
    - If the module/use companion plan has landed `-M` / `--module-path` support
      by this point, include one repeatable module-path smoke scenario. If not,
      keep module-path coverage deferred and name that deferral in the report.
    - Keep all still-unsupported constructs explicitly reported as deferred.
  - Full quality gates:
    - `cargo test -p asm motorola68020_opforge_native_cli -- --nocapture`
    - `cargo test -p asm external_fs_uae_opforge_native_cli_reports_module_use_parser_status -- --nocapture` with clean skip when not configured
    - `cargo fmt --all --check`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit --no-fetch`
    - `cargo test --workspace`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` before the
    commit with evidence that this slice only broadens smoke coverage.
  - Commit outcome: One commit that gives a compact native AmigaOS front-end
    corpus for future iteration.
  - Definition of done:
    - The smoke corpus proves the native VM pipeline over more than one line.
    - Module/use records are table-backed, not print-only scanner output, unless
      this roadmap has been explicitly updated to allow a narrower substitute.
    - Include expansion records are present and show the included file's logical
      source locations.
    - Unsupported features fail or defer with named statuses.
    - The emitter boundary remains explicit.

- [ ] Work item 11: decide the next native boundary after the VM front end works
  - Source requirement or finding IDs: roadmap exit criteria; avoid reinventing
    the next plan after the first portable VM pipeline exists.
  - Validation: See Full quality gates for this work item.
  - Definition of done: See detailed criteria below for this work item.
  - Expected files:
    - a new follow-up plan under `documentation/plans/`
    - this roadmap for final progress evidence only
  - Implementation slice:
    - Review the native VM pipeline evidence and choose the next smallest native
      objective: instruction selection/encoding, native symbol table, native
      directive VM, or Hunk emission integration.
    - Write a follow-up plan for exactly one chosen objective.
  - Full quality gates:
    - `python3 scripts/workflow/check_plan_checkboxes.py` for the new follow-up
      plan and this roadmap
    - plan-quality review for the new follow-up plan
    - `cargo fmt --all --check`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit --no-fetch`
    - `cargo test --workspace`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` before the
    commit with evidence that this item is planning-only.
  - Commit outcome: One commit that closes this roadmap and creates the next
    executable plan.
  - Definition of done:
    - The next plan has one clear native implementation objective.
    - The next plan starts after the verified `TKVM` -> host bootstrap -> `PRVM`
      -> `EXVM` -> emitter-boundary pipeline, not before it.

## Milestones

- [ ] Milestone 1: host-owned bootstrap scan is stable in the native CLI.
- [x] Milestone 1a: native module/use companion spec and plan are incorporated
  as the authoritative bootstrap track.
- [ ] Milestone 1b: table-backed `.module` and `.use` records are available for
  the first VM pipeline smoke.
- [x] Milestone 1c: native `.include` input expansion is specified and available
  before parser VM CLI integration.
- [ ] Milestone 2: native CLI has a durable front-end report/handoff contract,
  including module/use table summary records and include expansion records.
- [ ] Milestone 3: native `TKVM` plus native `PRVM` can parse a no-expression
  opasm statement from the CLI.
- [ ] Milestone 4: native `PRVM` can request a bounded expression range for an
  m68k operand shape.
- [ ] Milestone 5: native `EXVM` can parse the first scalar/arithmetic operand
  expression without Rust fallback.
- [ ] Milestone 6: native CLI can run include expansion -> `TKVM` -> host
  bootstrap -> `PRVM` -> `EXVM` -> emitter boundary for one realistic m68k
  source stream.
- [ ] Milestone 7: the native AmigaOS smoke corpus is broad enough to drive the
  next implementation plan.

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- do not touch unrelated in-flight implementation files unless the active work
  item explicitly owns them
- do not add `litellm` to commands, manifests, examples, docs, or generated
  guidance
