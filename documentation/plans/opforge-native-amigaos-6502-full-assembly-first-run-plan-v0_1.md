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
- Do not revert existing dirty worktree changes. Current local changes in
  `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm` and
  `crates/opforge-asm/src/tests.rs` must be preserved and treated as input
  context unless the user explicitly directs otherwise.
- Do not install, import, add, recommend, vendor, execute, or otherwise touch
  `litellm`.
- Keep the first implementation target `m6502`; 65C02, 65816, 45GS02, and other
  family variants remain out of scope except where package data is shared and
  not broadened by native logic.
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

- [ ] Item 6: Complete package-backed `m6502` selector and encoder parity
  - Source requirement or finding IDs: `SR-6502-SELECTOR`,
    `SR-6502-ENCODER`, `SR-RUST-VM-ARCH`, `SR-TKPKG-SERVICE`; expected to
    finish the first-run instruction matrix by routing native selection and
    byte emission through package-backed runtime data rather than native
    CPU-specific tables.
  - Scope boundary: Item 6 completeness means the native selector and encoder
    path is package backed end to end for the first-run `m6502` matrix. Do not
    add or expand native hard-coded `m6502` mnemonic dispatch, addressing-mode
    acceptance tables, opcode tables, branch-offset opcode logic, or CLI-side
    CPU-specific selection rules. Any transitional direct subset from earlier
    items may remain only as compatibility scaffolding until replaced, but Item
    6 must move completion evidence toward package/chunk/VM-driven lookup and
    must not deepen that scaffolding.
  - Expected files: `native/motorola68000/amigaos/opasm/*`,
    `native/motorola68000/amigaos/tkpkg/*`, package fixture references as
    needed, and focused tests in `crates/opforge-asm/src/tests.rs`. Expected
    native production changes should primarily teach the tkpkg/opasm service
    boundary to consume package selector/encoding records and selected operand
    bytes; CLI changes are allowed only for request/response wiring, not for
    selector or encoder semantics.
  - Intermediate execution slices are allowed only when each slice moves native
    behavior closer to package-backed selection or encoding without expanding
    direct native `m6502` logic. The first allowed slice is the MSEL locator and
    selected-envelope guard bridge: native package loading records the `MSEL`
    chunk, selected encode envelopes are checked against active-owner `MSEL`
    mnemonic/mode records when selector records are present, existing selected
    CLI outputs stay byte-compatible, and tests assert that no new native
    `m6502` edge-mode hardcodes were introduced. The next allowed slice is
    MSEL candidate construction for selected simple forms: native selected
    encoding classifies package selector shapes, chooses mode keys from active
    `MSEL` records, applies package operand plans for `none`, `u8`, and `u16`,
    emits the compact envelope from package data before falling back to the
    transitional selector stage, prefers package `TABL` programs for byte
    emission before any transitional direct encoder fallback, and keeps existing
    selected CLI bytes stable.
    The next implied-form slice is allowed to add package shape recognition for
    zero-operand selected instructions, use the existing `none` operand plan,
    and prove the native path selects the `implied` package shape before any
    selector-stage fallback, without adding opcode or mnemonic tables.
    The next parenthesized-indirect slice is allowed to classify clean `(expr)`
    operands as the package `indirect` shape while leaving comma-bearing
    indexed-indirect forms to later slices, and must reuse package operand
    plans plus `TABL` emission rather than adding direct opcode logic.
    Later slices must broaden MSEL operand-shape and operand-plan execution to
    the remaining edge forms, then broaden the package-backed parity corpus to
    those forms.
  - Full quality gates: focused selector/encoder tests for implied,
    accumulator, indirect, indexed-indirect, indirect-indexed, relative branch,
    and jump-indirect forms that prove those forms are selected and encoded
    from package/chunk/VM data, plus guard assertions that no new native
    `m6502` opcode or addressing-mode hard-code path was introduced; `cargo
    test -p vm vm_runtime_mos6502_ -- --nocapture`; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 6 slice
    summary, changed files, and validation output; require `PASS`.
  - Final Item 6 commit outcome: complete first-run `m6502` selector and
    encoder parity through package-backed runtime behavior and either remove or
    clearly bypass any previous direct native `m6502` selector/encoder fallback
    for the completed forms. Intermediate commits must use the slice evidence
    above and must not claim full Item 6 completion until the definition of done
    below is met.
  - Definition of done: native output bytes for the selected full first-run
    `m6502` corpus match Rust bytes, selected-form diagnostics come from the
    package-backed service path, and inspection/tests show no new native
    CPU-specific mnemonic, addressing-mode, opcode, or relative-branch encoding
    tables were added outside the package-backed runtime/VM data path.

- [ ] Item 7: Implement layout-control directives in native opasm
  - Source requirement or finding IDs: `SR-DIRECTIVES`,
    `SR-OPASM-ENGINE`; expected to support the directives that directly shape
    image layout before broader data emission support lands.
  - Expected files: `native/motorola68000/amigaos/opasm/*`,
    `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm` only for
    boundary wiring if needed, and directive parity tests.
  - Full quality gates: focused tests for `.org`, `.align`, `.fill`,
    `.res/.ds`, `.region`, `.section`, and `.place`; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; plus
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

- [ ] Item 8: Implement data and text emission directives in native opasm
  - Source requirement or finding IDs: `SR-DIRECTIVES`,
    `SR-OPASM-ENGINE`; expected to support direct byte, word, and text
    emission once layout control exists.
  - Expected files: `native/motorola68000/amigaos/opasm/*`,
    `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm` only for
    boundary wiring if needed, and directive parity tests.
  - Full quality gates: focused tests for `.byte/.db`, `.word/.dw`, `.long`,
    `.text`, `.null`, and `.ptext`; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 8 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that implements first-run data and text
    emission directives inside native opasm.
  - Definition of done: first-run fixtures can emit byte, word, long, and text
    data with Rust-compatible bytes and diagnostics.

- [ ] Item 9: Implement symbol and configuration directives in native opasm
  - Source requirement or finding IDs: `SR-DIRECTIVES`,
    `SR-OPASM-ENGINE`; expected to support first-run symbol/configuration state
    without expanding into source-graph behavior.
  - Expected files: `native/motorola68000/amigaos/opasm/*`,
    `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm` only for
    boundary wiring if needed, and directive parity tests.
  - Full quality gates: focused tests for `.const/.var/.set`, `.cpu`, and the
    first-run conditional behavior explicitly included in the acceptance matrix;
    `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`; plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 9 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that implements symbol/configuration
    directive execution needed by first-run 6502 fixtures.
  - Definition of done: symbol-setting, CPU selection, and scoped first-run
    conditional behavior match Rust-compatible state changes and diagnostics.

- [ ] Item 10: Implement include-root and file expansion source graph behavior
  - Source requirement or finding IDs: `SR-SOURCE-GRAPH`,
    `SR-CLI-BOUNDARY`; expected to support the first multi-file path.
  - Expected files: `native/motorola68000/amigaos/opasm/*`,
    `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm`, and focused
    Rust/native source graph tests.
  - Full quality gates: focused tests for include roots, `.include`, missing
    include diagnostics, and source line remapping; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 10 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that makes native include-file expansion
    match the Rust source graph behavior needed for first-run fixtures.
  - Definition of done: native can assemble first-run include-based fixtures
    with Rust-matching include resolution and diagnostics.

- [ ] Item 11: Implement module declaration and root resolution parity
  - Source requirement or finding IDs: `SR-SOURCE-GRAPH`,
    `SR-DIRECTIVES`; expected to establish module structure before import
    semantics land.
  - Expected files: `native/motorola68000/amigaos/opasm/*`,
    `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm`, FS-UAE fixture
    setup as needed, and focused source graph tests.
  - Full quality gates: focused tests for module roots, explicit and implicit
    modules, module diagnostics, and module line remapping; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; opt-in FS-UAE source
    graph smoke; plus `scripts/workflow/run_rust_quality_gate.sh`.
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
    imports/aliases, and missing/ambiguous module diagnostics; `cargo test -p
    asm motorola68020_opforge_native_cli_ -- --nocapture`; opt-in FS-UAE
    source graph smoke; plus `scripts/workflow/run_rust_quality_gate.sh`.
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
- [ ] Milestone 3: native `m6502` selector and encoder parity lands in two
  commit-sized slices covering baseline and edge addressing modes.
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
- 65C02, 65816, 45GS02, and broader MOS-family native completion.
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
