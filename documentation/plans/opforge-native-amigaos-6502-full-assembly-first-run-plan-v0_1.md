<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# opForge Native AmigaOS 6502 Full Assembly First Run Plan v0.1

## Metadata

- Source: User request on 2026-05-06 to inventory and plan the remaining work
  for the Amiga native opForge implementation to fully support assembling for
  6502 while following the Rust VM architecture; follow-up scope limits first
  run outputs to `.hex`, `.lst`, `.bin`, and `.prg`.
- Mode: implementation
- Owner: opForge implementation agent

## Source Artifact And Requirement Mapping

- Source artifact type: explicitly scoped user instruction and implementation
  inventory from 2026-05-06.
- Source artifact scope: complete the native 68020/AmigaOS opForge assembly
  path for 6502 with Rust VM architectural parity, but keep first-run output
  implementation limited to `.hex`, `.lst`, `.bin`, and `.prg`.

### Explicit Source Instruction And Implementation Inventory

- User instruction, 2026-05-06: inspect the current Amiga native opForge
  implementation and inventory what is needed to fully support assembling for
  6502, including outputs and directives, with full functionality following the
  architecture of the Rust VM path while focusing on 6502 first.
- User scope clarification, 2026-05-06: create the implementation plan from
  that inventory; on the output side, give the native implementation the
  appropriate architecture, but only `.hex`, `.lst`, `.bin`, and `.prg` need to
  be supported in the first run.
- Inventory point: the native CLI path must stop owning assembler semantics.
  It should keep AmigaDOS argument parsing, file IO, diagnostics, status codes,
  and host orchestration while delegating parse, assemble, encode, and output
  requests to the native runtime/engine layers.
- Inventory point: native `opasm` must become the assembly engine owner for
  6502 projects. It needs session state, pass control, source rows, symbol
  state, current PC/origin, image/range state, directive execution, diagnostics,
  and output artifact requests.
- Inventory point: the `tkpkg` service boundary must expose the same package
  model shape as the Rust VM path for tokenize, parse, encode, pipeline
  selection, and last-error reporting. The live native path should not bypass
  that boundary for parse or encode decisions.
- Inventory point: parser output must grow from smoke-level statement handling
  into directive-ready and operand-ready records. PRVM should describe statement
  shape and expression spans, while opcore/EXVM evaluates expressions through
  pass-aware callbacks.
- Inventory point: 6502 selector and encoder parity must be package/runtime
  driven. The CLI must not duplicate mnemonic or addressing-mode acceptance, and
  the native path must cover the full `m6502` instruction/addressing matrix.
- Inventory point: directive parity must cover the practical first-run 6502
  assembler surface, including origin/range control, byte/word/text emission,
  storage/fill behavior, constants/variables, CPU selection, module/use/include
  structure, and first-run conditional behavior.
- Inventory point: source graph behavior must support practical multi-file
  projects with include/module/use expansion following the Rust model where
  applicable.
- Inventory point: native output must become a distinct artifact subsystem fed
  by the assembled image/session. The first run implements `.bin`, `.prg`,
  `.hex`, and `.lst`; `.srec`, Amiga Hunk, map files, labels files, dependency
  files, and export-section outputs are deferred but should not require moving
  behavior back into CLI glue later.
- Inventory point: FS-UAE parity validation must compare native AmigaOS
  artifacts against Rust-generated reference artifacts for the first-run 6502
  matrix.

- Source requirement IDs used by this plan:
  - `SR-NATIVE-6502-FULL`: native AmigaOS opForge can assemble real 6502
    projects end to end, not only the current small smoke subset.
  - `SR-RUST-VM-ARCH`: native runtime layering follows the Rust VM path:
    package model, tokenizer, parser, opcore expression, opasm selector/encoder,
    assembly engine, and output artifacts remain distinct.
  - `SR-CLI-BOUNDARY`: CLI owns argument parsing, AmigaDOS file IO, exit codes,
    and user-facing diagnostics, but not assembler semantics.
  - `SR-OPASM-ENGINE`: native opasm owns assembly session state, pass control,
    statement rows, symbols, image state, and directive execution.
  - `SR-TKPKG-SERVICE`: package-backed runtime entrypoints remain the service
    boundary for tokenize, parse, encode, and last-error behavior.
  - `SR-6502-SELECTOR`: 6502 mnemonic/addressing-mode selection is
    package/runtime-driven, not hard-coded in the CLI.
  - `SR-6502-ENCODER`: 6502 byte emission uses the package-backed encoder
    surface and supports the full `m6502` instruction/addressing matrix.
  - `SR-EXPR-PARITY`: expression parsing/evaluation follows the opcore/EXVM
    architecture with pass-aware symbol, current-PC, and forward-reference
    behavior.
  - `SR-DIRECTIVES`: supported directive behavior matches the Rust assembler
    surface required for 6502 projects.
  - `SR-SOURCE-GRAPH`: native include/module/use expansion follows the Rust
    source graph model where applicable.
  - `SR-OUTPUT-ARCH`: native output code has an extensible artifact structure
    matching Rust output components.
  - `SR-FIRST-OUTPUTS`: first-run output support is limited to `.hex`, `.lst`,
    `.bin`, and `.prg`; other outputs remain planned but intentionally
    not implemented in this run.
  - `SR-FS-UAE-PARITY`: FS-UAE end-to-end tests compare native artifacts against
    Rust-generated references for the first-run 6502 matrix.
  - `SR-AGENTS-COMPLIANCE`: active worktree `AGENTS.md` workflow and execution
    rules remain binding throughout execution.

## Objective

Make the native 68020/AmigaOS opForge implementation a real 6502 assembler path
that follows the same architectural responsibilities as the Rust VM path. The
first run should be broad enough to assemble practical 6502 sources with
directives, labels, expressions, source graph behavior, and first-run artifact
outputs, while avoiding a premature attempt to implement every eventual output
format.

First-run output formats:

- `.bin`: raw binary output, including ranges and fill behavior needed for 6502.
- `.prg`: Commodore PRG output with a 16-bit little-endian load address prefix.
- `.hex`: Intel HEX output.
- `.lst`: listing output.

The output subsystem should still be structured as native artifact components so
later `.srec`, Amiga Hunk, map files, labels, dependency files, and export
section outputs can be added without moving behavior back into CLI glue.

## First-Run 6502 Acceptance Matrix

The first executable parity contract is
`examples/mos6502/6502_first_run_artifact_contract.asm`, with Rust references in
`examples/reference/mos6502/6502_first_run_artifact_contract.hex` and
`examples/reference/mos6502/6502_first_run_artifact_contract.lst`.

This fixture is the Item 1 acceptance target for the native AmigaOS path:

- CPU and pipeline selection: `.cpu 6502`.
- Origin/range control: placed `code` section in a `$0800..$083f` region, plus
  a `.bin` comparison range of `$0800..$0814`.
- Symbols and expressions: `.const`, `.var`, `sta $0200 + OFFSET`, and
  `.word start + 3`.
- Instruction/addressing coverage: immediate, absolute expression, relative
  forward branch, relative backward branch, and implied forms.
- Directive coverage: `.byte`, `.word`, `.text`, `.fill`, `.region`,
  `.section`, `.place`, and `.output`.
- First-run artifact outputs:
  - `.bin`: `A9 42 8D 02 02 F0 05 D0 F7 A2 10 E8 AA 0C 08 03 08 4F 4B FF FF`
    for `$0800..$0814`.
  - `.prg`: little-endian load address prefix `00 08` followed by the `.bin`
    payload.
  - `.hex`: Intel HEX record
    `:15080000A9428D0202F005D0F7A210E8AA0C0803084F4BFFFFB0` followed by EOF.
  - `.lst`: source/byte listing matching
    `examples/reference/mos6502/6502_first_run_artifact_contract.lst`.

The focused Rust-side contract test is
`motorola68020_opforge_native_cli_first_run_artifact_contract_locks_rust_outputs`.
Later native implementation items must make the AmigaOS CLI artifact outputs
match this Rust-reference contract before expanding the matrix.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- This plan must not become active until `plan-quality-reviewer` or
  `plan-quality-orchestrator` returns `PASS`.
- Execute one work item at a time; each work item ends in exactly one new commit
  before the next work item starts.
- Boundary update, 2026-06-07: the cross-family selector-boundary remediation
  plan has completed and was archived. Generic Rust runtime selector leaks for
  MOS, Intel, M65816, and M68K operand-surface parsing were moved behind
  family-owned seams, and the native tkpkg selector surface is now documented
  as one isolated table-driven transitional seam. Future work in this plan must
  preserve that boundary and must not reintroduce selector or encoder ownership
  into CLI glue, generic Rust VM code, or scattered native tag-name helpers.
- Rust-reference implementation rule, 2026-06-07: before inventing native
  logic for parser, selector, encoder, directive, source-graph, image, or
  output behavior, implementation slices must identify the existing Rust path
  that already solves the same problem and use it as the behavioral guide. A
  native slice may diverge from that guide only for concrete differences between
  Rust and a native 68020/AmigaOS codebase, such as memory layout, calling
  convention, register pressure, table representation, fixed-buffer handling,
  host file IO, or assembly control flow. Divergence must preserve the Rust
  semantics, be recorded in the slice summary or plan evidence, and must not
  create a second independent selector, encoder, directive, source-graph, image,
  or output policy when the Rust implementation already owns that policy.
- Do not install, import, add, recommend, vendor, execute, or otherwise touch
  `litellm`.
- Keep the first implementation target focused on the attached MOS package
  fixture set: base `m6502` plus the attached 65C02 parity fixtures. 65C02
  support in this plan is included only through package/Rust-VM-backed selector
  and encoder data; it must not introduce 65C02-specific native selector or
  encoder code. 65816, 45GS02, and other family variants remain out of scope
  except where package data is shared and not broadened by native logic.
- `.cpu` and `.org` may be replaced by hard-coded test harness setup while Item
  6 is being completed. This exception does not authorize hard-coded native
  CPU-specific mnemonic, addressing-mode, selector, operand-plan, relative
  branch, or opcode logic.
- Do not implement `.srec`, Amiga Hunk, map files, labels files, dependency
  files, or export-section outputs in this first run, but do not design the
  first-run output code in a way that blocks them.
- Keep CPU-specific selection and encoding out of opcore. Opcore remains the
  generic expression processor.
- Keep parser VM responsibility limited to statement/operand shape and
  expression request boundaries. Do not move math evaluation into PRVM.
- All first-run native output behavior must have Rust-reference parity tests.

## Version Impact

- Affected component(s): native AmigaOS opForge CLI, native opasm engine,
  native tkpkg service, native PRVM/opcore integration, native output artifact
  modules, FS-UAE smoke/parity tests, Rust reference fixtures for 6502.
- Impact class: minor
- Owned contract: native AmigaOS opForge 6502 assembly behavior and first-run
  artifact output parity with the Rust VM path.
- Rationale: the current native implementation has advanced from tokenizer and
  small-assembly smoke support toward the right module layout, but full 6502
  assembly still needs native opasm engine ownership, full selector/encoder
  parity, directive/source graph support, and output components.

## Architecture Target

The intended native shape mirrors the Rust path:

- CLI host: parse arguments, open/read/write files through AmigaDOS, choose
  package, pass output requests to the engine, print diagnostics, and return
  status codes.
- Native opasm engine: own assembly session state, source rows, pass control,
  symbol table, current PC/origin, image model, directive execution, and output
  artifact requests.
- tkpkg service: expose stable runtime entrypoints for package load,
  set-pipeline, tokenize-line, parse-line, encode-instruction, and last-error.
- tokenizer VM: tokenize selected `m6502` source lines from package data.
- PRVM/opasm parser: produce statement/directive/operand-shape records and
  expression span requests.
- opcore/EXVM: parse and evaluate generic expressions with symbol/current-PC
  callbacks.
- opasm selector/encoder: select full `m6502` operand candidates and emit bytes
  through package-backed runtime data.
- native output artifacts: render `.bin`, `.prg`, `.hex`, and `.lst` from the
  assembled image/session without reaching into parse or encode internals.

## Work Items

- [x] Item 1: Freeze the first-run 6502 parity matrix and artifact contract
  - Source requirement or finding IDs: `SR-NATIVE-6502-FULL`,
    `SR-FIRST-OUTPUTS`, `SR-FS-UAE-PARITY`; expected to establish the
    executable acceptance target.
  - Expected files: `documentation/plans/opforge-native-amigaos-6502-full-assembly-first-run-plan-v0_1.md`,
    focused Rust/native test declarations in `crates/opforge-asm/src/tests.rs`,
    and small 6502 fixtures under `examples/mos6502/` or the existing native
    FS-UAE fixture path.
  - Full quality gates: focused `cargo test -p asm motorola68020_opforge_native_cli_
    -- --nocapture`, focused Rust reference tests for `.hex`, `.lst`, `.bin`,
    and `.prg` outputs, `make workflow-gate` or the relevant workflow plan
    validators for the modified plan artifact, plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 1 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that locks the first-run fixture set,
    expected native CLI surface, expected output artifacts, and Rust-reference
    comparison strategy.
  - Definition of done: the plan has concrete 6502 source cases covering
    labels, expressions, directives, branches, and `.hex`/`.lst`/`.bin`/`.prg`
    outputs, with no production behavior changes beyond test/fixture contract
    setup.

- [x] Item 2: Move remaining assembly-engine ownership out of the CLI
  - Source requirement or finding IDs: `SR-CLI-BOUNDARY`,
    `SR-OPASM-ENGINE`, `SR-RUST-VM-ARCH`; expected to make the native engine
    boundary real.
  - Expected files: `native/motorola68000/amigaos/opasm/*`,
    `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm`, and focused
    coverage in `crates/opforge-asm/src/tests.rs`.
  - Full quality gates: focused `cargo test -p asm motorola68020_opforge_native_cli_
    -- --nocapture`, focused `cargo test -p asm motorola68020_prvm_ --
    --nocapture`, focused `cargo test -p asm motorola68020_tkpkg_ --
    --nocapture`, plus `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 2 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that gives native opasm ownership of
    assembly session state, pass loop, statement rows, labels, image buffer, and
    PC/origin state while the CLI remains only the host/orchestrator.
  - Definition of done: the CLI calls a native opasm engine entrypoint instead
    of directly owning pass/session/image semantics.
  - Completion note, 2026-05-07: native `opasm.amigaos.engine` now owns the
    transitional two-pass loop and assembly-session storage for statement rows,
    labels, image bytes, source records, current PC, origin, and session pass.
    The CLI imports that state and supplies host callbacks for the existing
    smoke path until Item 3 moves broader parse records through the package
    boundary.

- [x] Item 3: Complete package-backed parse records for directives and operands
  - Source requirement or finding IDs: `SR-TKPKG-SERVICE`,
    `SR-DIRECTIVES`, `SR-EXPR-PARITY`; expected to broaden parse output from
    smoke statements into directive-ready engine rows.
  - Expected files: `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm`,
    `native/motorola68000/amigaos/prvm/*`,
    `native/motorola68000/amigaos/opasm/*`, and parser parity tests in
    `crates/opforge-asm/src/tests.rs`.
  - Full quality gates: focused `cargo test -p vm parser_vm_v2_parity --
    --nocapture`, focused `cargo test -p asm motorola68020_prvm_ --
    --nocapture`, focused `cargo test -p asm motorola68020_tkpkg_ --
    --nocapture`, plus `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 3 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that makes parse records sufficient for
    the first-run directive and operand set without adding CLI-side re-parsing.
  - Definition of done: native opasm receives enough structured row data to
    execute first-run directives and instructions from PRVM/tkpkg service output.
  - Completion note, 2026-05-07: native PRVM now emits distinct
    `directive_text` and `operand_text` result records through the tkpkg
    parse-line service boundary. The native CLI consumes those parse records to
    distinguish directives, copy operand spans into opasm statement rows, and
    store statement directive-kind metadata in opasm-owned session storage
    without adding new CLI-side mnemonic or operand re-parsing.

- [x] Item 4: Implement full VM-based expression processing parity for first-run
  6502 assembly
  - Source requirement or finding IDs: `SR-EXPR-PARITY`,
    `SR-RUST-VM-ARCH`; expected to make expression processing pass-aware,
    parser/evaluator-driven, and engine-owned.
  - Scope boundary: Item 4 covers scalar operand-expression forms needed by the
    first-run 6502 matrix. Item 5 starts only after the
    `evaluate-expression` boundary has produced resolved operand values.
  - Expected files: `native/motorola68000/amigaos/opcore/*`,
    `native/motorola68000/amigaos/opasm/*`,
    `native/motorola68000/amigaos/opforge-cli/*`,
    `native/motorola68000/amigaos/tkpkg/*`, and focused VM/ASM tests.
  - Full quality gates: focused VM-expression-processing tests for the
    service-backed parse/eval boundary across literals, unary/binary
    operations, current PC, constants, labels, forward references, branch
    offsets, parse failures, and error cases; `cargo test -p vm
    vm_opcore_expression_ -- --nocapture`; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; `cargo test -p asm
    motorola68020_tkpkg_ -- --nocapture`; plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: `PASS` on the dedicated staged Item 4
    completion slice after excluding Item 5 select/encode-selected work.
  - Commit outcome: exactly one completion commit that replaces the remaining
    compatibility service-backed path with package-backed EXVM/EXPR
    parse/eval for first-run 6502 scalar expressions, leaving no authoritative
    CLI-side operand parsing or direct bridge-only evaluation path.
  - Completed selector/service slice: native opasm selector-stage operand
    resolution is callback-based, and the native CLI routes both selector
    requests and stored-statement operand reads through
    `ENTRY_ORD_EVALUATE_EXPRESSION` instead of directly calling
    `opcore_expr_eval_operand_v1` from the CLI or selector stage.
  - Completed persistence slice: native opasm session storage now keeps
    per-statement expression-slot flags, slot ids, token bounds, span metadata,
    source-line text, and label-finalized state so later evaluation can reload
    the original operand slice with pass-aware symbol-finalization behavior.
  - Completed ABI/service slice: tkpkg now exposes an evaluate-expression ABI
    envelope plus an extension window for label-table, symbol-value,
    current-PC, and result-slot exchange, and the current service
    implementation resolves EXPR/EXVM contract versions before dispatching
    through `opcore_exvm_eval_operand_v1`.
  - Completed EXVM-parity slice: the native opcore bridge covers the current
    first-run 6502 scalar subset for `*`, unary `+`/`-`, chained
    additive/subtractive terms, `$`/`0x` hex literals, `%` binary literals,
    decimal literals, and label/constant-table symbols.
  - Validation evidence: `cargo test -p vm vm_opcore_expression_ --
    --nocapture`; `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`;
    `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`; and
    `./scripts/workflow/run_rust_quality_gate_summary.sh` all passed for the
    governed closure work.
  - Item 4 closure note, 2026-05-11: this dedicated completion commit closes
    Item 4. The remaining unstaged selector/select-instruction and
    encode-selected work is explicitly Item 5 scope and is not part of this
    closure slice.
  - Definition of done: `.org`, operands, constants, labels, current-PC
    references, forward-reference resolution, and branch-offset behavior match
    the Rust VM path for the first-run scalar matrix, with native expression
    text processed through the same VM-style package parse/eval boundary rather
    than native ad hoc parsing or bridge-only evaluation.

- [x] Item 5: Implement baseline `m6502` selector parity for data-bearing modes
  - Source requirement or finding IDs: `SR-6502-SELECTOR`,
    `SR-6502-ENCODER`; expected to remove the largest remaining
    tiny-subset limitation without taking on every edge mode at once.
  - Expected files: `native/motorola68000/amigaos/opasm/*`,
    `native/motorola68000/amigaos/opforge-cli/*`,
    `native/motorola68000/amigaos/tkpkg/*`, and focused tests in
    `crates/opforge-asm/src/tests.rs`.
  - Scope note: Item 4 owns expression-value production through the native
    evaluate-expression boundary. Item 5 starts after that boundary and focuses
    on turning resolved operand values into baseline `m6502` selection and
    encoding decisions for the first-run data-bearing families.
  - Completion note, 2026-05-12: Item 5 now routes native pass-two emission
    through the shared line/span request, `ENTRY_ORD_SELECT_INSTRUCTION`, and
    `ENTRY_ORD_ENCODE_SELECTED_INSTRUCTION`, so selection and selected-form
    encoding cross the tkpkg boundary before selector-stage policy runs. The
    tkpkg/native selector stage distinguishes resolved zero-page versus absolute
    forms, parses simple `,X`/`,Y` indexed suffixes for the baseline
    data-bearing families, and feeds those concrete selections back into
    pass-one sizing through the package-backed selected-encode path. The focused
    6502 smoke contract and FS-UAE native-output path cover immediate,
    zero-page, zero-page indexed, absolute, absolute indexed, and jump bytes in
    the same fixture.
  - Validation evidence, 2026-05-12: `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture` passed 5 tests; `cargo
    test -p asm motorola68020_tkpkg_ -- --nocapture` passed 31 tests; `cargo
    test -p vm vm_runtime_mos6502_ -- --nocapture` passed 10 tests; the opt-in
    FS-UAE smoke `external_fs_uae_opforge_native_cli_6502_writes_rust_matching_bin`
    passed and wrote a Rust-matching `opforge_native_out.bin`; and
    `scripts/workflow/run_rust_quality_gate_summary.sh` passed.
  - Full quality gates: focused selector/encoder tests for immediate,
    zero-page, zero-page indexed, absolute, and absolute indexed forms through
    package-backed select/encode-selected surfaces; `cargo test -p vm
    vm_runtime_mos6502_ -- --nocapture`; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; `cargo test -p asm
    motorola68020_tkpkg_ -- --nocapture`; the opt-in FS-UAE native CLI output
    smoke when host FS-UAE access is available; plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 5 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that moves baseline `m6502`
    data-bearing mode selection and selected-form encoding onto the native
    package-backed selector/encoder boundary, removing CLI-side mnemonic and
    addressing-mode policy for the core immediate/zero-page/absolute families.
  - Definition of done: once Item 4 supplies resolved operand values through
    the native expression service boundary, native select/encode flow emits
    Rust-matching bytes for the first-run corpus that uses immediate,
    zero-page, zero-page indexed, absolute, and absolute indexed addressing
    families without hard-coded CLI acceptance logic.

- [x] Item 6: Complete package-backed MOS 6502/65C02 selector and encoder
  parity
  - Source requirement or finding IDs: `SR-6502-SELECTOR`,
    `SR-6502-ENCODER`, `SR-RUST-VM-ARCH`, `SR-TKPKG-SERVICE`; expected to
    finish the attached MOS instruction/addressing fixture matrix by routing
    native selection and byte emission through package-backed runtime data
    rather than native CPU-specific tables.
  - Status correction, 2026-05-13: the earlier Item 6 completion note proved
    fallback removal and partial package-backed plumbing only. It did not prove
    that native opForge can assemble the full attached 6502/65C02 instruction
    and addressing-mode fixtures through package-backed processing. Item 6 is
    therefore reopened and decomposed into the subitems below.
  - Scope boundary: Item 6 completeness means the native selector and encoder
    path is package backed end to end for the attached fixture set:
    `examples/mos6502/6502_native_cli_smoke.asm`,
    `examples/mos6502/6502_simple.asm`,
    `examples/mos6502/6502_allmodes.asm`,
    `examples/mos6502/65c02_simple.asm`, and
    `examples/mos6502/65c02_allmodes.asm`. Do not add or expand native
    hard-coded MOS mnemonic dispatch, addressing-mode acceptance tables, opcode
    tables, operand-plan tables, branch-offset opcode logic, or CLI-side
    CPU-specific selection rules. If native CPU-specific selector or encoder
    logic appears necessary in any area other than hard-coded `.cpu`/`.org` test
    setup, stop and ask the user for approval before implementing it.
  - Boundary impact update, 2026-06-07: the completed cross-family selector
    remediation work closes the broad generic-runtime boundary risk that was
    still open when Item 6 was decomposed. MOS selector normalization now lives
    in family-owned runtime code, M68K operand-surface parsing is behind a
    family hook, Intel and M65816 selector decisions are no longer open-coded in
    generic selector runtime files, and
    `documentation/architecture/cpu-specific-arch-boundary.md` records the
    native tkpkg selector surface as a single table-driven transitional seam.
    Item 6 remains about proving the Amiga-native MOS package-backed path and
    removing or containing native selector/encoder residue; it no longer needs
    to rediscover the already remediated cross-family Rust boundary issues.
  - Rust-reference guardrail, 2026-06-07: Item 6 native selector and encoder
    changes must start from the Rust package selector/encoder implementation,
    package data, and VM tests as the reference behavior. Do not add native
    shape inference, branch sizing, pair-operand handling, candidate admission,
    or byte-emission logic because it appears quicker than tracing the Rust
    path. Native-only code may exist only as an implementation translation of
    the Rust/package behavior into 68020 assembly service boundaries, fixed
    buffers, and AmigaOS host constraints, and the validation evidence must make
    that translation visible through same-package identity checks and labeled
    Rust/native byte output.
  - Expected files: `native/motorola68000/amigaos/opasm/*`,
    `native/motorola68000/amigaos/tkpkg/*`, package fixture references as
    needed, and focused tests in `crates/opforge-asm/src/tests.rs`. Expected
    native production changes should primarily teach the tkpkg/opasm service
    boundary to consume package selector/encoding records and selected operand
    bytes; CLI changes are allowed only for request/response wiring and the
    explicitly allowed `.cpu`/`.org` test setup, not for selector or encoder
    semantics.
  - Same-package-bytecode validation rule: every Item 6 byte-parity test must
    feed native execution and Rust golden generation from the exact same
    serialized package bytecode payload in the same test run. The tests must
    record or assert the package identity, such as a byte-for-byte equality or
    hash check, before comparing output bytes. Native tests must not use a
    separately generated, independently embedded, stale, or hand-edited package
    when the Rust golden bytes come from a different package payload.
  - Human-readable byte evidence rule: every Item 6 parity gate must emit or
    record clear hexadecimal byte output showing both sides for each checked
    source row or fixture. The evidence must label the source row or fixture,
    the Rust VM bytes, and the native bytes, for example `rust: A9 42` and
    `native: A9 42`. A boolean equality assertion alone is not sufficient
    evidence for completing an Item 6 subitem.
  - Full quality gates: focused selector/encoder tests for implied,
    accumulator, indirect, indexed-indirect, indirect-indexed, relative branch,
    and jump-indirect forms that prove those forms are selected and encoded
    from the same package bytecode consumed by the Rust golden path and print
    labeled Rust/native hex bytes, plus guard assertions that no new native MOS
    opcode or addressing-mode hard-code path was introduced; `cargo test -p vm
    vm_runtime_mos6502_ -- --nocapture`; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 6 slice
    summary, changed files, and validation output; require `PASS`.
  - Final Item 6 commit outcome: complete attached MOS fixture selector and
    encoder parity through package-backed runtime behavior and remove or prove
    unreachable any previous direct native MOS selector/encoder fallback for the
    completed forms. Intermediate commits must use the subitem evidence below
    and must not claim full Item 6 completion until every subitem definition of
    done is met.
  - Definition of done: native output bytes for all indicated 6502 and 65C02
    fixture rows match Rust VM bytes while using the exact same serialized
    package bytecode payload as the Rust golden generator, selected-form
    diagnostics come from the package-backed service path, and inspection/tests
    show no native CPU-specific mnemonic, addressing-mode, opcode,
    operand-plan, or relative-branch encoding tables exist outside
    package-backed runtime/VM data and the explicitly approved `.cpu`/`.org`
    test setup.
  - Subitem execution rule: each Item 6 subitem below is a commit-sized slice.
    Execute one subitem at a time, run plan-compliance review before its commit,
    and do not advance to the next subitem while its focused validation is red.
  - [x] Item 6.1: Lock native-vs-Rust byte parity harness for the indicated MOS
    fixtures
    - Source requirement or finding IDs: `SR-6502-SELECTOR`,
      `SR-6502-ENCODER`, `SR-FS-UAE-PARITY`; expected to create the executable
      measurement target for the rest of Item 6.
    - Expected files: `crates/opforge-asm/src/tests.rs`, FS-UAE/native CLI
      manifest helpers if needed, same-package byte identity checks, and fixture
      allowlists that name only the five indicated MOS fixtures.
    - Full quality gates: focused Rust oracle test that renders exact bytes for
      `6502_native_cli_smoke.asm`, `6502_simple.asm`, `6502_allmodes.asm`,
      `65c02_simple.asm`, and `65c02_allmodes.asm`; focused native harness test
      that can run the same sources with hard-coded `.cpu`/`.org` setup where
      needed and report per-line native byte mismatches; required human-readable
      evidence that prints each checked source row or fixture with `rust:` and
      `native:` hexadecimal byte sequences; a required assertion that native
      execution and Rust golden generation use the exact same serialized package
      bytes; existing guard test that rejects native MOS selector/encoder
      hardcodes; plus `scripts/workflow/run_rust_quality_gate.sh` before commit.
    - Plan-compliance review evidence: before commit, run
      `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 6.1
      slice summary, changed files, and validation output; require `PASS`.
    - Commit outcome: exactly one commit that makes the attached fixture matrix
      measurable against Rust VM bytes without claiming full native pass status.
    - Definition of done: the harness reports exact expected byte sequences for
      every instruction row in the five indicated fixtures, prints labeled
      Rust/native hexadecimal bytes for human review, proves the native and Rust
      oracle paths consumed the same package bytecode payload, and clearly
      separates allowed `.cpu`/`.org` setup from selector/encoder behavior.
    - Validation evidence, 2026-05-13: `cargo test -p asm
      motorola68020_item6_1_locks_mos_fixture_byte_parity_harness --
      --nocapture` passed and printed per-row `rust:`/`native:` byte evidence
      for all five allowed fixtures with row-level same-package byte identity
      assertions; `cargo test -p asm
      motorola68020_item6_does_not_expand_native_m6502_edge_hardcodes --
      --nocapture` passed; `scripts/workflow/run_rust_quality_gate_summary.sh`
      passed. The evidence harness intentionally reports current native byte
      mismatches without claiming full Item 6 parity completion.
    - FS-UAE evidence, 2026-05-13: `OPFORGE_FS_UAE_SMOKE=1 ... cargo test
      -p asm external_fs_uae_opforge_native_cli_6502_writes_rust_matching_bin
      -- --nocapture --test-threads=1` executed the native AmigaOS CLI through
      tokenizer, parser, pass1, pass2, and session reporting, then failed at
      `ERROR OPC-NCLI009: native emitter VM not implemented` with
      `SESSION-IMAGE-BYTES 0`. This is accepted for Item 6.1 because the slice
      locks the byte-parity measurement harness and does not claim native CLI
      output parity; full FS-UAE exact-byte parity remains Item 6.7.
  - [x] Item 6.2: Wire parser/request selector-shape handoff into native
    selected encode
    - Source requirement or finding IDs: `SR-6502-SELECTOR`,
      `SR-TKPKG-SERVICE`, `SR-RUST-VM-ARCH`; expected to unblock `MSEL` lookup
      without native operand spelling classification.
    - Expected files: native PRVM/opasm request records,
      `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm` only for
      request-window wiring, `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm`,
      and focused tests.
    - Full quality gates: focused native selected-request tests proving the
      service receives package selector shapes from the same serialized package
      bytecode payload used by the Rust golden generator for `implied`,
      `accumulator`, `immediate`, `direct`, `direct_x`, `direct_y`, `indirect`,
      `indexed_indirect_x`, and `indirect_indexed_y`; exact-byte parity for the
      current smoke fixture with labeled Rust/native hex evidence; hardcode
      guard test; native 68000 formatter check if supported ASM changes; plus
      `scripts/workflow/run_rust_quality_gate.sh`.
    - Plan-compliance review evidence: before commit, run
      `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 6.2
      slice summary, changed files, and validation output; require `PASS`.
    - Commit outcome: exactly one commit that extends the native selected
      request extension or equivalent service envelope so `tkpkg` can consume
      Rust/package selector shapes and expression spans.
    - Definition of done: `tkpkgBuildSelectedEnvelopeFromMselV1` no longer sees
      missing shape metadata for the smoke and simple fixture forms, and native
      code still contains no MOS-specific shape classifier.
    - Validation evidence, 2026-05-13: `cargo test -p asm
      motorola68020_item6_2_preserves_package_selector_shapes_for_smoke_and_required_shapes
      -- --nocapture` passed and printed labeled `rust:`/`native:` hexadecimal
      byte evidence for `6502_native_cli_smoke.asm` plus the required
      selector-shape matrix: `implied`, `accumulator`, `immediate`, `direct`,
      `direct_x`, `direct_y`, `indirect`, `indexed_indirect_x`, and
      `indirect_indexed_y`, with same-package byte identity asserted before
      each fixture comparison. `cargo test -p asm
      motorola68020_item6_2_native_cli_preserves_parser_spans_for_selected_requests
      -- --nocapture` passed, proving selected requests preserve parser span
      metadata and use source-line text when metadata exists. `cargo test -p
      asm motorola68020_item6_does_not_expand_native_m6502_edge_hardcodes --
      --nocapture` passed; `cargo test -p asm
      motorola68020_item6_1_locks_mos_fixture_byte_parity_harness --
      --nocapture` passed; `scripts/workflow/run_native_68000_format_gate.sh`
      passed; `scripts/workflow/run_rust_quality_gate_summary.sh` passed.
  - [x] Item 6.3: Complete generic single-operand selected emission plans
    - Source requirement or finding IDs: `SR-6502-ENCODER`,
      `SR-TKPKG-SERVICE`; expected to cover package-backed `none`, `u8`, `u16`,
      and `rel8` selected forms.
    - Expected files: `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm`,
      native expression/current-PC bridge code only if required, and focused
      tests.
    - Full quality gates: focused selected-encode tests for implied,
      accumulator, immediate, zero-page, zero-page indexed, absolute, absolute
      indexed, indirect, indexed-indirect, indirect-indexed, and relative branch
      rows using the same package bytecode payload for native and Rust golden
      execution; branch tests must compare signed offsets against Rust VM
      output and print labeled Rust/native hex bytes for each checked row;
      hardcode guard test; native formatter check; plus
      `scripts/workflow/run_rust_quality_gate.sh`.
    - Plan-compliance review evidence: before commit, run
      `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 6.3
      slice summary, changed files, and validation output; require `PASS`.
    - Commit outcome: exactly one commit that teaches native `tkpkg` to execute
      the package operand plans needed by base 6502 single-operand rows without
      opcode or mnemonic special cases.
    - Definition of done: native selected bytes match Rust VM bytes for all
      base-6502 rows in `6502_simple.asm` and the non-branch plus branch rows
      selected from `6502_allmodes.asm`.
    - Validation evidence, 2026-05-13: `cargo test -p asm
      motorola68020_item6_3_covers_generic_single_operand_selected_emission_plans
      -- --nocapture` passed and printed labeled `rust:`/`native:` hex bytes
      for package-backed `none`, `u8`, `u16`, and `rel8` selected emission
      plans across implied/accumulator, immediate, zero-page, zero-page indexed,
      absolute, absolute indexed, indirect, indexed-indirect,
      indirect-indexed, and relative branch rows. Branch evidence included
      `bcc branch_test` as `rust: 90 FE` / `native: 90 FE` and `bne forward`
      as `rust: D0 08` / `native: D0 08`, with same-package byte identity
      asserted before each fixture comparison. `cargo test -p asm
      motorola68020_item6_3_native_tkpkg_implements_rel8_as_generic_plan --
      --nocapture` passed, proving native `tkpkg` handles `rel8` as a generic
      package plan using `EncodeSelectedCurrentPc`. `cargo test -p asm
      motorola68020_item6_does_not_expand_native_m6502_edge_hardcodes --
      --nocapture` passed; `scripts/workflow/run_native_68000_format_gate.sh`
      passed; `scripts/workflow/run_rust_quality_gate_summary.sh` passed.
  - [x] Item 6.4: Execute package `TABL` bytecode with operand-index parity
    - Source requirement or finding IDs: `SR-6502-ENCODER`,
      `SR-RUST-VM-ARCH`; expected to make native byte emission obey package
      programs rather than an operand-zero shortcut.
    - Expected files: `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm`
      and focused tests.
    - Full quality gates: tests proving `OP_EMIT_U8`, `OP_EMIT_OPERAND index`,
      and `OP_END` behavior matches Rust VM for operand index 0 and later
      operand indexes when both paths use the same package bytecode payload,
      with labeled Rust/native hex evidence for representative programs;
      negative tests for malformed program/index references;
      existing 6502 smoke parity; native formatter check; plus
      `scripts/workflow/run_rust_quality_gate.sh`.
    - Plan-compliance review evidence: before commit, run
      `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 6.4
      slice summary, changed files, and validation output; require `PASS`.
    - Commit outcome: exactly one commit that makes native `TABL` execution
      consume the package bytecode operand index field generically.
    - Definition of done: native `TABL` execution can emit every operand byte
      requested by package bytecode programs needed by the attached base 6502
      and 65C02 fixture rows.
    - Validation evidence, 2026-05-13: `cargo test -p asm
      motorola68020_item6_4 -- --nocapture` passed. It printed labeled
      same-package Rust/native TABL evidence for a patched MOS package program
      using `OP_EMIT_U8`, `OP_EMIT_OPERAND 0`, `OP_EMIT_OPERAND 1`, and
      `OP_END`: `rust: A9 11 2C 22 33` / `native: A9 11 2C 22 33`. The same
      focused run passed negative malformed/index cases for missing literal
      byte, missing operand index, operand index 1 with one operand, and an
      invalid opcode, with matching Rust/native error text. The native source
      contract test proved `tkpkgEncodeExecuteProgram` now walks candidate
      operand records by bytecode index instead of rejecting nonzero indexes.
      `cargo test -p asm
      motorola68020_item6_does_not_expand_native_m6502_edge_hardcodes --
      --nocapture` passed. `cargo test -p asm
      motorola68020_item6_1_locks_mos_fixture_byte_parity_harness --
      --nocapture` passed and retained existing 6502/65C02 smoke parity,
      including two-operand 65C02 bit-branch rows. `scripts/workflow/
      run_native_68000_format_gate.sh --write` reported 0 changed files after
      formatting; `scripts/workflow/run_native_68000_format_gate.sh` passed;
      `scripts/workflow/run_rust_quality_gate_summary.sh` passed.
  - [x] Item 6.5: Pass base 6502 simple and all-modes exact-byte fixture parity
    - Source requirement or finding IDs: `SR-6502-SELECTOR`,
      `SR-6502-ENCODER`, `SR-NATIVE-6502-FULL`; expected to complete base 6502
      package-backed selector/encoder coverage for the attached fixtures.
    - Expected files: native opasm/tkpkg implementation files and focused
      parity tests only.
    - Full quality gates: exact native-vs-Rust byte parity for
      `6502_native_cli_smoke.asm`, `6502_simple.asm`, and `6502_allmodes.asm`;
      mandatory same-package byte identity check before every parity comparison;
      required human-readable evidence listing each fixture or row with `rust:`
      and `native:` hexadecimal bytes; guard tests that reject native MOS
      opcode/mode/mnemonic/branch tables;
      `cargo test -p vm vm_runtime_mos6502_ -- --nocapture`; native formatter
      check if applicable; plus `scripts/workflow/run_rust_quality_gate.sh`.
    - Plan-compliance review evidence: before commit, run
      `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 6.5
      slice summary, changed files, and validation output; require `PASS`.
    - Commit outcome: exactly one commit that makes all attached base 6502
      instruction/addressing rows emit Rust-matching bytes through package
      selection and `TABL` execution.
    - Definition of done: the three attached base 6502 fixtures pass exact byte
      parity with native and Rust golden paths consuming the same serialized
      package bytecode payload, the validation evidence includes labeled
      Rust/native hexadecimal output, and there is no native CPU-specific
      selector or encoder logic.
    - Validation evidence, 2026-05-13: `cargo test -p asm
      motorola68020_item6_5_base_6502_fixtures_match_exact_native_and_rust_bytes
      -- --nocapture` passed and printed labeled `rust:`/`native:` hex bytes
      for every instruction row in `6502_native_cli_smoke.asm`,
      `6502_simple.asm`, and `6502_allmodes.asm`, with same-package byte
      identity asserted before every fixture and row comparison. Representative
      exact branch evidence included `bcs $+2` as `rust: B0 00` / `native: B0
      00`, `beq $+4` as `rust: F0 02` / `native: F0 02`, `bne forward` as
      `rust: D0 08` / `native: D0 08`, and the other `$+2` base-6502 branch
      rows as matching `30 00`, `10 00`, `50 00`, and `70 00` bytes. `cargo
      test -p vm vm_runtime_mos6502_ -- --nocapture` passed with 10 VM MOS6502
      tests. `cargo test -p asm
      motorola68020_item6_does_not_expand_native_m6502_edge_hardcodes --
      --nocapture` passed. `scripts/workflow/run_native_68000_format_gate.sh`
      passed with 35 checked files, 0 would change, and 0 warnings.
      `scripts/workflow/run_rust_quality_gate_summary.sh` passed.
  - [x] Item 6.6: Add generic two-operand and 65C02 package-plan support
    - Source requirement or finding IDs: `SR-6502-SELECTOR`,
      `SR-6502-ENCODER`, `SR-RUST-VM-ARCH`; expected to cover the attached
      65C02 fixture rows while keeping native logic CPU-neutral.
    - Expected files: native parser/request boundary, `tkpkg` operand-plan
      interpreter, and focused tests.
    - Full quality gates: package-backed selected-encode tests for 65C02-only
      forms in `65c02_simple.asm` and `65c02_allmodes.asm`, including `BRA`,
      `BBR`/`BBS` pair operands, zero-page indirect, and absolute indexed
      indirect; exact branch-offset parity against Rust VM with the same package
      bytecode payload on both paths; labeled Rust/native hex evidence for each
      checked 65C02-only row; hardcode guard test; native formatter check; plus
      `scripts/workflow/run_rust_quality_gate.sh`.
    - Plan-compliance review evidence: before commit, run
      `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 6.6
      slice summary, changed files, and validation output; require `PASS`.
    - Commit outcome: exactly one commit that implements package operand plans
      such as two-operand byte/relative forms generically, not as 65C02-native
      special cases.
    - Definition of done: native selected encode emits Rust-matching bytes for
      all 65C02-only rows in the attached simple and all-modes fixtures while
      consuming the same serialized package bytecode payload as the Rust golden
      path, validation evidence includes labeled Rust/native hexadecimal output,
      and there are no native 65C02 mnemonic/opcode/addressing tables.
    - Validation evidence, 2026-05-13: `cargo test -p asm
      motorola68020_item6_6_65c02_package_plans_match_exact_native_and_rust_bytes
      -- --nocapture` passed and printed labeled `rust:`/`native:` hex bytes
      for every row in `65c02_simple.asm` and `65c02_allmodes.asm`, with
      same-package byte identity asserted before every fixture and row
      comparison. Required 65C02-only evidence included `bra skip` as `rust: 80
      02` / `native: 80 02`, `bbr0 $20, bbr_target` as `rust: 0F 20 01` /
      `native: 0F 20 01`, `bbs7 $21, bbs_target` as `rust: FF 21 01` /
      `native: FF 21 01`, zero-page indirect `lda ($20)` as `rust: B2 20` /
      `native: B2 20`, and absolute indexed indirect `jmp ($1234,x)` as
      `rust: 7C 34 12` / `native: 7C 34 12`. `cargo test -p asm
      motorola68020_item6_does_not_expand_native_m6502_edge_hardcodes --
      --nocapture` passed. `scripts/workflow/run_native_68000_format_gate.sh`
      passed with 35 checked files, 0 would change, and 0 warnings.
      `scripts/workflow/run_rust_quality_gate_summary.sh` passed.
  - [x] Item 6.7: Prove native CLI/FS-UAE exact byte parity for the full
    indicated fixture set
    - Source requirement or finding IDs: `SR-FS-UAE-PARITY`,
      `SR-NATIVE-6502-FULL`, `SR-6502-ENCODER`; expected to promote row-level
      selected parity into native CLI end-to-end evidence.
    - Expected files: FS-UAE/native CLI tests, fixture manifests, and native CLI
      request wiring only if needed for `.cpu`/`.org` test setup.
    - Full quality gates: opt-in FS-UAE native CLI exact-byte parity for all
      five indicated fixtures when host FS-UAE access is available; non-FS-UAE
      deterministic harness parity for the same fixture set; both gates must
      assert that the native package payload is byte-identical to the package
      payload used by the Rust golden generator and print labeled Rust/native
      hexadecimal bytes for each fixture; `cargo test -p asm
      motorola68020_opforge_native_cli_ -- --nocapture`; native formatter check
      if applicable; plus `scripts/workflow/run_rust_quality_gate.sh`.
    - Plan-compliance review evidence: before commit, run
      `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 6.7
      slice summary, changed files, and validation output; require `PASS`.
    - Commit outcome: exactly one commit that records full indicated-fixture
      native CLI parity evidence and keeps `.cpu`/`.org` hardcoded setup limited
      to the approved test exception.
    - Definition of done: native CLI output bytes match Rust VM bytes for the
      full indicated fixture set while using the exact same serialized package
      bytecode payload as the Rust golden path, validation evidence includes
      labeled Rust/native hexadecimal output, and any FS-UAE skip is reported as
      host-access skip rather than a project pass.
    - Interim host evidence, 2026-05-13: `cargo test -p asm
      motorola68020_item6_7_full_indicated_fixture_native_cli_parity_matches_rust_bytes
      -- --nocapture` passed and printed per-fixture labeled native CLI bin
      payload evidence for `6502_native_cli_smoke.asm`, `6502_simple.asm`,
      `6502_allmodes.asm`, `65c02_simple.asm`, and `65c02_allmodes.asm`. The
      deterministic native path initializes the native package service, consumes
      the same package bytes as the Rust golden path, parses each source line
      through the native line parser, runs native selected encode in first and
      second pass, stores bytes in a CLI-style image, and compares the resulting
      flat `.bin` payload against the Rust payload. Representative bin evidence
      included `6502_native_cli_smoke.asm` as `rust bin: A9 42 85 20 B5 20 8D
      00 02 BD 00 02 B9 00 02 4C 0F 08 FF` / matching `native bin: ...`,
      `65c02_simple.asm` as `rust bin: 64 20 9C 34 12 80 02 EA EA DA 5A FA 7A
      1A 3A 14 30 04 40 90 0E B0 0C F0 0A D0 08 30 06 10 04 50 02 70 00 A9
      42 A2 10 A0 20 89 55 69 01 29 0F 09 F0 49 AA A5 50 85 60 60 FF` /
      matching `native bin: ...`, and `65c02_allmodes.asm` as matching bin
      bytes including `0F 20 01`, `FF 21 01`, `B2 20`, `7C 34 12`, and the
      trailing fill byte `FF`. `cargo test -p asm motorola68020_opforge_native_cli_
      -- --nocapture` passed with 5 tests.
      `cargo test -p asm
      external_fs_uae_opforge_native_cli_6502_writes_rust_matching_bin --
      --nocapture --test-threads=1` passed as a clean opt-in skip with `SKIP:
      set OPFORGE_FS_UAE_SMOKE=1 to enable the opt-in FS-UAE smoke test`, so
      no host FS-UAE execution was claimed. `scripts/workflow/
      run_native_68000_format_gate.sh` passed with 35 checked files, 0 would
      change, and 0 warnings. `scripts/workflow/run_rust_quality_gate_summary.sh`
      passed.
    - Validation evidence, 2026-06-13: commit `3261becd` completed the native
      Item 6.7 package-backed path. The implementation removed the temporary
      native direct bit-branch helper and restored byte emission through
      `tkpkgEncodeFindAndExecuteTableProgram`, so `BBR`/`BBS` behavior remains
      encoded in package table/program data rather than CPU-specific native
      helper logic. `cargo test -p asm
      motorola68020_item6_7_full_indicated_fixture_native_cli_parity_matches_rust_bytes
      -- --nocapture` passed and printed matching full `.bin` payloads for
      all five indicated fixtures. `OPFORGE_FS_UAE_SMOKE=1
      OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae'
      OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/
      Configurations/opforge-tkpkg-test.fs-uae'
      OPFORGE_FS_UAE_ARGS='{fsuae_config}'
      OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=120000 cargo test -p asm
      external_fs_uae_opforge_native_cli_item6_stripped_fixtures_match_rust_bins
      -- --nocapture --test-threads=1` passed in 83.78s and printed matching
      `rust bin:` / `native bin:` evidence for `6502_native_cli_smoke.asm`,
      `6502_simple.asm`, `6502_allmodes.asm`, `65c02_simple.asm`, and
      `65c02_allmodes.asm`. The `65c02_allmodes.asm` FS-UAE evidence included
      matching `0F 20 01` and `FF 21 01` bytes for `BBR0`/`BBS7`, plus
      matching `B2 20` and `7C 34 12` bytes for the other 65C02-only rows.
      `cargo test -p asm
      motorola68020_item6_does_not_expand_native_m6502_edge_hardcodes --
      --nocapture` passed after the helper removal, `git diff --check` passed,
      `scripts/workflow/run_native_68000_format_gate.sh --write` reported 0
      changed files after formatting, and `scripts/workflow/
      run_rust_quality_gate.sh` completed with `PASS: Rust quality gate
      complete.`
  - [x] Item 6.8: Final native CPU-specific selector/encoder audit and removal
    - Source requirement or finding IDs: `SR-CLI-BOUNDARY`,
      `SR-RUST-VM-ARCH`, `SR-6502-SELECTOR`, `SR-6502-ENCODER`; expected to
      close Item 6 without native MOS-specific selector or encoder residue.
    - Expected files: native opasm/tkpkg/opforge-cli sources and guard tests.
    - Full quality gates: source-inspection guard tests rejecting active native
      MOS mnemonic tables, addressing-mode tables, opcode tables, relative
      branch opcode logic, selector-stage fallback calls, and raw operand
      spelling classifiers outside package/VM data; guard tests proving the
      tkpkg surface lookup remains isolated behind tables and the embedded
      native CLI package retains required 65C02 bit-branch selector/table
      entries; exact-byte parity tests from Items 6.5 through 6.7, including
      their same-package-bytecode identity checks and labeled Rust/native
      hexadecimal byte evidence; `cargo test -p vm generic_selector_runtime_ --
      --nocapture`; `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`;
      native formatter check; plus `scripts/workflow/run_rust_quality_gate.sh`.
    - Plan-compliance review evidence: before commit, run
      `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 6.8
      slice summary, changed files, validation output, and a statement that no
      unapproved native CPU-specific selector/encoder code remains; require
      `PASS`.
    - Commit outcome: exactly one commit that removes or proves unreachable any
      remaining native MOS selector/encoder fallback and records the no
      CPU-specific-native-code audit evidence.
    - Definition of done: Item 6 may be marked complete only after all Item 6
      subitems pass, the full indicated fixture set emits Rust-matching bytes,
      and inspection/tests show no native CPU-specific selector/encoder code
      remains except user-approved `.cpu`/`.org` test setup.
    - Interim implementation evidence: the active implementation consumes Rust-generated
      package chunks for parser routing and selected encoding: PRVM records are
      located by `tkpkgPipelineSetActiveV1`, the CLI loads active PRVM bytecode
      from package storage, MSEL records are scanned without a caller-supplied
      native shape requirement, and TABL bytecode executes selected candidates.
      The obsolete native selector-stage fallback module
      `native/motorola68000/amigaos/opasm/opasm_selector_stage.asm` is deleted.
      The native service retains only generic package-shape consumption for
      candidate operand preprocessing (`immediate`, `direct_x`, `direct_y`) and
      does not add a native MOS mnemonic table, addressing-mode table, opcode
      table, relative-branch opcode table, selector-stage fallback call, or raw
      operand spelling classifier outside package/VM data.
    - Boundary impact update, 2026-06-07: the separate cross-family boundary
      remediation plan completed and archived after moving MOS selector
      normalization, Intel selector synthesis, M65816 selector state/plans, and
      M68K operand-surface admission behind family-owned seams. The remaining
      Item 6.8 audit is therefore narrowed to this native first-run path: prove
      no unapproved native MOS selector/encoder residue remains, prove the
      documented tkpkg table-driven transitional seam stays isolated, and carry
      the new generic-runtime guardrails as regression protection.
    - Interim host validation evidence: `cargo test -p asm
      motorola68020_item6_5_base_6502_fixtures_match_exact_native_and_rust_bytes
      -- --nocapture` passed with labeled Rust/native hexadecimal byte parity
      for the base 6502 fixture corpus. `cargo test -p asm
      motorola68020_item6_6_65c02_package_plans_match_exact_native_and_rust_bytes
      -- --nocapture` passed with labeled Rust/native hexadecimal byte parity
      for the 65C02 fixture corpus. `cargo test -p asm
      motorola68020_item6_7_full_indicated_fixture_native_cli_parity_matches_rust_bytes
      -- --nocapture` passed and printed matching full `.bin` payloads for
      `6502_native_cli_smoke.asm`, `6502_simple.asm`, `6502_allmodes.asm`,
      `65c02_simple.asm`, and `65c02_allmodes.asm`. `cargo test -p asm
      motorola68020_tkpkg_ -- --nocapture` passed with 31 tests. `cargo test
      -p asm motorola68020_item6_does_not_expand_native_m6502_edge_hardcodes --
      --nocapture` passed. `scripts/workflow/run_native_68000_format_gate.sh`
      passed with 35 checked files, 0 would change, and 0 warnings. A direct
      `scripts/workflow/run_rust_quality_gate.sh` invocation could not return
      readable terminal output in this VS Code session, so the repository-
      approved summary wrapper was used; `scripts/workflow/
      run_rust_quality_gate_summary.sh` completed successfully and reported
      `PASS: Rust quality gate complete.` after logging the full gate to
      `target/workflow-logs/rust-quality-gate.log`.
    - Status update, 2026-06-13: Item 6.8 is unblocked by the green Item 6.7
      host FS-UAE proof. The remaining work is the final audit/removal proof
      that no unapproved native MOS selector/encoder residue remains.
    - Validation evidence, 2026-06-13: the final audit removed native
      BBR/BBS/BRA mnemonic-based selected-shape classifiers from
      `opasm_engine.asm`, added generic top-level-comma detection so
      comma-separated operand requests do not get misclassified as scalar
      `direct`, and kept pair-operand evaluation in `tkpkg_service.asm`
      package-plan driven by clearing/restoring selected shape and mode state
      around each pair part. The native pair plan now uses the engine pass
      source for pass-one sizing, matching the Rust VM label layout while
      leaving BBR/BBS semantics in package MSEL/TABL data. `cargo test -p asm
      motorola68020_item6_8_native_shape_inference_has_no_mos_mnemonic_classifiers
      -- --nocapture` passed; `cargo test -p asm
      motorola68020_item6_does_not_expand_native_m6502_edge_hardcodes --
      --nocapture` passed; `cargo test -p asm
      motorola68020_item6_5 -- --nocapture`, `cargo test -p asm
      motorola68020_item6_6 -- --nocapture`, and `cargo test -p asm
      motorola68020_item6_7_full_indicated_fixture_native_cli_parity_matches_rust_bytes
      -- --nocapture` passed with labeled Rust/native hexadecimal evidence.
      `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/
      Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/
      Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae'
      OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test -p asm
      external_fs_uae_opforge_native_cli_item6_stripped_fixtures_match_rust_bins
      -- --nocapture --test-threads=1` passed in 83.52s; the
      `65c02_allmodes.asm` target evidence showed pass-one labels
      `bbr_target $00000822` and `bbs_target $00000826` and matching bytes
      including `0F 20 01`, `FF 21 01`, `B2 20`, and `7C 34 12`. `cargo test
      -p vm generic_selector_runtime_ -- --nocapture` passed with 5 tests;
      `cargo test -p asm motorola68020_tkpkg_ -- --nocapture` passed with 31
      tests; `scripts/workflow/run_native_68000_format_gate.sh --write`
      reported 0 changed files after formatting; `git diff --check` passed;
      `scripts/workflow/run_rust_quality_gate.sh` completed with `PASS: Rust
      quality gate complete.` This completes Item 6 because all Item 6
      subitems are now checked and the native MOS selector/encoder path has no
      unapproved native CPU-specific selector/encoder residue.

- Boundary carry-forward for Items 7 through 18, 2026-06-07: the recent
  boundary remediation does not reduce the remaining directive, source-graph,
  output, or final FS-UAE scope. It does tighten how those steps must be
  implemented: new directive/source/output behavior belongs in native opasm,
  source graph, or output-artifact layers, with CLI changes limited to request
  wiring and host IO. Any future touch to generic Rust VM selector/parser
  runtime or native tkpkg selector surfaces must preserve the family-owned and
  table-driven boundary documented in
  `documentation/architecture/cpu-specific-arch-boundary.md` and should include
  the relevant `generic_selector_runtime_`, `motorola68020_tkpkg_`, and
  CPU-specific architecture-boundary guard evidence in addition to the
  item-specific gates.
- Rust-reference carry-forward for Items 7 through 18, 2026-06-07: directive,
  source-graph, image, and output slices must treat the Rust implementation and
  Rust reference tests as the design source. Before adding native behavior,
  identify the Rust module or reference test that defines the behavior; copy the
  semantics, diagnostics, ordering, and edge cases unless a real native 68020 or
  AmigaOS constraint requires a different representation. Record any such
  representation difference in the slice evidence, and add parity or guard
  coverage so later agents do not replace Rust-guided behavior with an
  independently invented native policy.
- [x] Item 7: Implement layout-control directives in native opasm
  - Source requirement or finding IDs: `SR-DIRECTIVES`,
    `SR-OPASM-ENGINE`; expected to support the directives that directly shape
    image layout before broader data emission support lands.
  - Expected files: `native/motorola68000/amigaos/opasm/*`,
    `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm` only for
    boundary wiring if needed, and directive parity tests.
  - Full quality gates: focused tests for `.org`, `.align`, `.fill`,
    `.res/.ds`, `.region`, `.section`, and `.place`; boundary regression check
    when request wiring touches tkpkg or generic VM selector/parser surfaces;
    `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`; plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 7 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that implements layout-control
    directive execution inside native opasm, including section/region/placement
    state required by the first-run fixture contract.
  - Definition of done: first-run fixtures can use the listed data-placement
    directives to control origin, regions, sections, placement, alignment,
    fill, and reserved ranges with Rust-compatible state changes and
    diagnostics.
  - Validation evidence, 2026-06-13: native opasm now executes the first-run
    layout-control slice in `opasm_assembly_driver.asm` with region and
    section tables, duplicate/overlap/capacity checks, pass-one placement
    state, pass-two placed-section rebasing, and zero-fill gap materialization
    for flat native CLI image output. `opasm_engine.asm` added
    `opasmEngineSetCurrentPcV1` so later placed sections can advance the pass
    PC without overwriting the first image origin. Directive routing keeps
    `.org`, `.align`, `.fill`, `.res/.ds`, `.region`, `.section`, and `.place`
    ahead of selected instruction emission, and the Item 7 source-shape test
    locks those routes plus the placed-section gap/origin helper.
    `cargo test -p asm item7 -- --nocapture` passed. `OPFORGE_FS_UAE_SMOKE=1
    OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae'
    OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/
    Configurations/opforge-tkpkg-test.fs-uae'
    OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test -p asm
    external_fs_uae_opforge_native_cli_item7_layout_directives_match_rust_guided_bytes
    -- --nocapture --test-threads=1` passed in 49.60s and verified native
    AmigaOS output bytes `A9 01 00 00 EA 00 00 00 00 00 00 00 00 00 00 00 00
    A9 02`, `SESSION-ORIGIN $00001002`, missing-region failure, and duplicate
    placement failure. `cargo test -p asm motorola68020_opforge_native_cli_ --
    --nocapture` passed with 5 tests. `scripts/workflow/
    run_native_68000_format_gate.sh --write` and `scripts/workflow/
    run_native_68000_format_gate.sh` both passed with 0 native formatting
    changes. `python3 scripts/workflow/check_cpu_specific_arch_boundary.py`
    passed with no enforced-scope errors. `scripts/workflow/
    run_rust_quality_gate.sh` completed with `PASS: Rust quality gate
    complete.` Plan-compliance reviewer rubric was applied with `AGENTS.md`,
    this plan, the Item 7 slice summary, changed files, and validation output;
    result: `PASS` for a slice limited to native opasm layout-control
    execution and directive parity tests.

- [x] Item 8: Implement data and text emission directives in native opasm
  - Source requirement or finding IDs: `SR-DIRECTIVES`,
    `SR-OPASM-ENGINE`; expected to support direct byte, word, and text
    emission once layout control exists.
  - Expected files: `native/motorola68000/amigaos/opasm/*`,
    `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm` only for
    boundary wiring if needed, and directive parity tests.
  - Full quality gates: focused tests for `.byte/.db`, `.word/.dw`, `.long`,
    `.text`, `.null`, and `.ptext`; boundary regression check when request
    wiring touches tkpkg or generic VM selector/parser surfaces; `cargo test -p
    asm motorola68020_opforge_native_cli_ -- --nocapture`; plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 8 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that implements first-run data and text
    emission directives inside native opasm.
  - Definition of done: first-run fixtures can emit byte, word, long, and text
    data with Rust-compatible bytes and diagnostics.
  - Validation evidence, 2026-06-14: native opasm now routes `.byte/.db`,
    `.word/.dw`, `.long`, `.text`, `.null`, and `.ptext` ahead of selected
    instruction emission in `opasm_assembly_driver.asm`, using the Rust data
    and text directive behavior in `asmline_directives_data.rs` and
    `asmline_directives_text.rs` as the semantic source. Numeric directive
    operands reuse the native opasm comma operand evaluator and emit MOS
    first-run little-endian bytes; text directives parse quoted literal bytes
    into opasm-owned scratch storage, with `.null` zero-byte rejection and
    `.ptext` length-prefix bounds. Focused structural test
    `motorola68020_item8_native_data_text_directives_route_before_selected_encoding`
    locks directive routing and helper ownership. Focused FS-UAE fixture
    `external_fs_uae_opforge_native_cli_item8_data_text_directives_match_rust_guided_bytes`
    passed outside the sandbox after the sandboxed FS-UAE launcher aborted with
    SIGABRT, and verified exact native bytes `01 FF 02 34 12 00 08 FE 00 78
    56 34 12 4F 4B 41 00 02 42 43`. `cargo test -p asm item8 --
    --nocapture` passed with the FS-UAE case skipped when the opt-in
    environment was absent. `cargo test -p asm motorola68020_opforge_native_cli_
    -- --nocapture` passed with 5 tests. `scripts/workflow/
    run_native_68000_format_gate.sh --write` passed with 0 changed files.
    `python3 scripts/workflow/check_cpu_specific_arch_boundary.py` passed with
    no enforced-scope errors. `git diff --check` passed. `scripts/workflow/
    run_rust_quality_gate.sh` completed with `PASS: Rust quality gate
    complete.` Plan-compliance reviewer returned `PASS: Item 8 scope matches
    the plan; changes are limited to native opasm directive emission and
    focused parity tests, required validations are reported passing, and the
    commit boundary is focused.`

- [x] Item 9: Implement symbol and configuration directives in native opasm
  - Source requirement or finding IDs: `SR-DIRECTIVES`,
    `SR-OPASM-ENGINE`; expected to support first-run symbol/configuration state
    without expanding into source-graph behavior.
  - Expected files: `native/motorola68000/amigaos/opasm/*`,
    `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm` only for
    boundary wiring if needed, and directive parity tests.
  - Full quality gates: focused tests for `.const/.var/.set`, `.cpu`, and the
    first-run conditional behavior explicitly included in the acceptance matrix;
    boundary regression check when CPU-selection request wiring touches tkpkg or
    generic VM selector/parser surfaces; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 9 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that implements symbol/configuration
    directive execution needed by first-run 6502 fixtures.
  - Definition of done: symbol-setting, CPU selection, and scoped first-run
    conditional behavior match Rust-compatible state changes and diagnostics.
  - Validation evidence, 2026-06-14: native opasm now routes `.const`,
    `.var`, and `.set` ahead of selected instruction emission, evaluates their
    scalar operand through the existing pass-aware opcore/tkpkg expression
    service, and stores the resulting symbol value in the opasm-owned symbol
    table. Value-backed symbols are finalized immediately so data directives
    can resolve exact symbol operands in pass 2 before falling back to package
    expression evaluation. `.var` and `.set` use a narrow update path for
    first-run mutable symbol assignment, while `.const` keeps duplicate-label
    diagnostics through the existing label event surface. `.cpu` remains
    accepted as a configuration directive that emits no bytes and does not
    advance the PC; first-run CPU/package selection remains host-selected
    through the existing native CLI `--cpu`/package pipeline boundary,
    preserving the current package-backed selector ownership. Conditional
    directives are still scoped to the existing native CLI preprocessing
    boundary and produce `OPC-NCLI015`; the first-run acceptance matrix does
    not yet include a conditional source fixture that reaches native opasm, and
    the Item 9 static test locks that current diagnostic boundary rather than
    expanding into source-graph behavior. Focused structural test
    `motorola68020_item9_native_symbol_config_directives_route_before_selected_encoding`
    passed and locks directive routing, symbol-value storage, `.cpu` no-output
    handling, data directive symbol resolution, and the conditional
    preprocessing boundary. Focused FS-UAE fixture
    `external_fs_uae_opforge_native_cli_item9_symbol_config_directives_match_rust_guided_bytes`
    is present for `.cpu`, `.const`, `.var`, `.set`, and symbol-backed
    data emission. `cargo test -p asm item9 -- --nocapture` passed with 2
    tests, with FS-UAE skipped by default. The mandatory real FS-UAE run
    `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/
    Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/
    Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae'
    OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test -p asm
    external_fs_uae_opforge_native_cli_item9_symbol_config_directives_match_rust_guided_bytes
    -- --nocapture --test-threads=1` passed and produced native bytes
    `42 a9 42 8d 02 02` with `SESSION-CPU m6502`,
    `SESSION-LABEL-COUNT 3`, and `SESSION-IMAGE-BYTES 6`. `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture` passed with 5 tests.
    `cargo fmt --check` passed. `python3
    scripts/workflow/check_cpu_specific_arch_boundary.py` passed with no
    enforced-scope errors. `git diff --check` passed. `scripts/workflow/
    run_rust_quality_gate.sh` completed with `PASS: Rust quality gate
    complete.` `make workflow-gate` passed, including agent symlink,
    supply-chain ban, CPU-specific architecture boundary, quality-gate
    evidence, reference scope, and release-note policy checks.

- [x] Item 10: Implement include-root and file expansion source graph behavior
  - Source requirement or finding IDs: `SR-SOURCE-GRAPH`,
    `SR-CLI-BOUNDARY`; expected to support the first multi-file path.
  - Expected files: `native/motorola68000/amigaos/opasm/*`,
    `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm`, and focused
    Rust/native source graph tests.
  - Full quality gates: focused tests for include roots, `.include`, missing
    include diagnostics, and source line remapping; boundary regression check if
    source graph request plumbing touches generic VM parser/selector runtime;
    `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`; plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 10 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that makes native include-file expansion
    match the Rust source graph behavior needed for first-run fixtures.
  - Definition of done: native can assemble first-run include-based fixtures
    with Rust-matching include resolution and diagnostics.
  - Completion evidence, 2026-06-14: implemented native CLI `-I` /
    `--include-path` recording, include-root probing in CLI order after the
    current file root, missing include diagnostics, and recursive include
    tokenization that preserves the outer source handle. The focused static
    test `cargo test -p asm item10 -- --nocapture` passed with the opt-in
    FS-UAE case skipped by default. The mandatory real FS-UAE run
    `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/
    Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/
    Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae'
    OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test -p asm
    external_fs_uae_opforge_native_cli_item10_include_roots_match_rust_guided_bytes
    -- --nocapture --test-threads=1` passed. That run selected
    `Work:opforge_include_root_b/defs.inc` ahead of root A, emitted
    Rust-guided bytes `22 a9 44`, and verified the missing include path reports
    `ERROR OPC-NCLI014: native include expansion failed`.
  - Gate evidence, 2026-06-14: `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture` passed;
    `scripts/workflow/run_rust_quality_gate.sh` completed with
    `PASS: Rust quality gate complete.` Item 10 intentionally did not add Item
    11 module declaration/root semantics or Item 12 `.use` import behavior.

- [ ] Item 11: Implement module declaration and root resolution parity
  - Source requirement or finding IDs: `SR-SOURCE-GRAPH`,
    `SR-DIRECTIVES`; expected to establish module structure before import
    semantics land.
  - Expected files: `native/motorola68000/amigaos/opasm/*`,
    `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm`, FS-UAE fixture
    setup as needed, and focused source graph tests.
  - Full quality gates: focused tests for module roots, explicit and implicit
    modules, module diagnostics, and module line remapping; boundary regression
    check if source graph request plumbing touches generic VM parser/selector
    runtime; `cargo test -p asm motorola68020_opforge_native_cli_ --
    --nocapture`; opt-in FS-UAE source graph smoke; plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 11 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that establishes module declaration and
    root resolution behavior required for first-run 6502 projects.
  - Definition of done: native can assemble first-run multi-file fixtures using
    `.module` with Rust-compatible module structure, line attribution, and
    diagnostics.

- [ ] Item 12: Implement `.use` and import-resolution source graph parity
  - Source requirement or finding IDs: `SR-SOURCE-GRAPH`;
    expected to finish the import-resolution behavior on top of Items 10 and 11.
  - Expected files: `native/motorola68000/amigaos/opasm/*`, FS-UAE fixture
    setup, and Rust/native source graph tests.
  - Full quality gates: focused tests for recursive `.use`, selected
    imports/aliases, and missing/ambiguous module diagnostics; boundary
    regression check if import-resolution plumbing touches generic VM
    parser/selector runtime; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; opt-in FS-UAE source
    graph smoke; plus `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 12 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that adds `.use` and import-resolution
    behavior for the first-run source graph surface.
  - Definition of done: native `.use` behavior matches Rust references for the
    first-run multi-file 6502 fixtures.

- [ ] Item 13: Add native output artifact architecture and `.bin` parity
  - Source requirement or finding IDs: `SR-OUTPUT-ARCH`,
    `SR-FIRST-OUTPUTS`, `SR-DIRECTIVES`; expected to establish the native
    output component shape with the simplest binary artifact first.
  - Expected files: new or existing `native/motorola68000/amigaos/opasm/*`
    output modules, `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm`,
    and artifact tests.
  - Full quality gates: focused native/Rust parity tests for `.bin` ranges,
    fill behavior, `.output` directive request handling, and path/default-name
    selection; `cargo test -p asm motorola68020_opforge_native_cli_ --
    --nocapture`; opt-in FS-UAE output smoke for `.bin`; plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 13 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that introduces native output artifact
    modules, routes first-run `.output` requests into that layer, and
    implements `.bin` output parity.
  - Definition of done: `.bin` is rendered through output artifact code, not
    CLI internals, `.output` selects the requested first-run artifact through
    the same layer, and native output matches Rust references for first-run
    cases.

- [ ] Item 14: Add `.prg` output parity
  - Source requirement or finding IDs: `SR-OUTPUT-ARCH`,
    `SR-FIRST-OUTPUTS`; expected to layer Commodore load-address output on top
    of the artifact subsystem after `.bin` is proven.
  - Expected files: native output artifact module files, focused tests, and
    FS-UAE expected-output fixtures.
  - Full quality gates: focused native/Rust parity tests for `.prg` load
    address prefix, path/default-name behavior, and wide load-address rejection;
    `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`;
    opt-in FS-UAE `.prg` output smoke; plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 14 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that adds native `.prg` output through
    the artifact layer.
  - Definition of done: native `.prg` output matches Rust references for the
    first-run fixtures.

- [ ] Item 15: Add `.hex` output parity
  - Source requirement or finding IDs: `SR-OUTPUT-ARCH`,
    `SR-FIRST-OUTPUTS`; expected to add Intel HEX as a first-run artifact.
  - Expected files: native output artifact module files, focused tests, and
    FS-UAE expected-output fixtures.
  - Full quality gates: focused native/Rust parity tests for Intel HEX records,
    sparse ranges, EOF record, and path/default-name behavior; `cargo test -p
    asm motorola68020_opforge_native_cli_ -- --nocapture`; opt-in FS-UAE `.hex`
    output smoke; plus `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 15 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that adds native `.hex` output through
    the artifact layer.
  - Definition of done: native `.hex` output for first-run 6502 fixtures matches
    Rust output text exactly or differs only where a documented line-ending
    normalization rule allows it.

- [ ] Item 16: Add `.lst` listing output parity
  - Source requirement or finding IDs: `SR-OUTPUT-ARCH`,
    `SR-FIRST-OUTPUTS`; expected to complete first-run output support.
  - Expected files: native output/listing module files, tests, and first-run
    listing references.
  - Full quality gates: focused native/Rust parity tests for addresses, bytes,
    source text, labels/directives, and multi-file source line attribution;
    `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`;
    opt-in FS-UAE `.lst` output smoke; plus `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 16 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that adds first-run `.lst` output through
    the artifact layer.
  - Definition of done: native `.lst` output matches the first-run Rust listing
    contract for the selected 6502 fixture set.

- [ ] Item 17: Add FS-UAE first-run artifact parity coverage
  - Source requirement or finding IDs: `SR-FS-UAE-PARITY`,
    `SR-NATIVE-6502-FULL`; expected to make the end-to-end acceptance harness
    complete for the first-run artifacts.
  - Expected files: `crates/opforge-asm/src/fs_uae_smoke.rs`,
    `crates/opforge-asm/src/tests.rs`, and native reference artifacts as
    needed.
  - Full quality gates: focused FS-UAE native CLI tests for `.bin`, `.prg`,
    `.hex`, and `.lst`; focused `cargo test -p asm
    examples_match_reference_outputs -- --nocapture`; plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 17 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that adds the missing FS-UAE coverage
    and reference-artifact wiring for the first-run output matrix.
  - Definition of done: FS-UAE can execute and compare the first-run 6502
    output matrix end to end for `.bin`, `.prg`, `.hex`, and `.lst`.

- [ ] Item 18: Record final first-run parity evidence and workflow-gate pass
  - Source requirement or finding IDs: `SR-FS-UAE-PARITY`,
    `SR-NATIVE-6502-FULL`, `SR-AGENTS-COMPLIANCE`; expected to close the plan
    with acceptance evidence rather than more feature scope.
  - Expected files: `crates/opforge-asm/src/tests.rs`, native fixtures and
    reference artifacts only if a deliberate acceptance-fixture refresh is
    needed, and any required workflow artifact updates.
  - Full quality gates: focused FS-UAE native CLI tests for `.hex`, `.lst`,
    `.bin`, and `.prg`; focused `cargo test -p asm
    examples_match_reference_outputs -- --nocapture`; `make workflow-gate`;
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 18 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that records final acceptance evidence,
    any intentionally deferred output notes, and a passing workflow gate.
  - Definition of done: the first-run 6502 matrix has passing FS-UAE parity and
    workflow-gate evidence recorded without adding new feature scope.

## Milestones

- [x] Milestone 1: first-run 6502 acceptance matrix is locked.
- [x] Milestone 2: CLI no longer owns assembly semantics; native opasm owns
  engine/pass/session behavior.
- [x] Milestone 3: native MOS 6502/65C02 selector and encoder parity lands in
  package-backed commit-sized slices covering the indicated simple, smoke, and
  all-modes fixtures.
- [ ] Milestone 4: first-run directive support lands in three commit-sized
  slices covering layout control, data/text emission, and
  symbol/configuration directives.
- [ ] Milestone 5: first-run source graph support lands in three commit-sized
  slices covering include expansion, module structure, and `.use` behavior.
- [ ] Milestone 6: `.bin`, `.prg`, `.hex`, and `.lst` are implemented through
  native artifact components.
- [ ] Milestone 7: FS-UAE end-to-end parity coverage and final workflow-gate
  evidence pass for the first-run 6502 matrix.

## Deferred Work

- `.srec` output.
- Amiga Hunk output.
- map files.
- labels files and JSON label output.
- dependency files.
- export-section outputs.
- 65816, 45GS02, and broader MOS-family native completion beyond the indicated
  base 6502 and 65C02 fixture set.
- LSP/fixit/editor-specific surfaces.
- broader optimizer or performance work beyond keeping the first-run native
  implementation practical on AmigaOS.

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no plan execution starts until the plan-quality reviewer returns `PASS`
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
