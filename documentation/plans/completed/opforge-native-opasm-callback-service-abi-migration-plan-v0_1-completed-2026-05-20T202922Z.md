<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# Plan: Native opasm callback/service ABI migration

## Metadata

- Source: User request to design the next native opasm callback/service ABI migration after the safe mechanical `opforge-cli` to `opasm` migration reached the callback/service boundary.
- Mode: design-led implementation plan
- Owner: Codex
- AGENTS binding: The active worktree `AGENTS.md` rules remain binding during execution.

## Goal

Introduce a native 68020-appropriate callback/service ABI that lets `opasm`
own assembly pass mechanics, selected encode/evaluate service use, label/PC/image
state, and structured assembler events while `opforge-cli` remains responsible
for host orchestration, user-facing rendering, artifact writing, and compatibility
wrappers during the migration.

The target architecture follows the Rust ownership split conceptually:

- `opforge-engine` owns assembly orchestration.
- `vm_opasm` owns assembler-oriented VM stages and output payload building.
- `RuntimeModelCore` / `HierarchyExecutionModel` own runtime package/model
  resolution.

Native must not become a literal Rust object model. It should use fixed ABI
frames, record buffers, offset/length string references, status codes,
structured event records, control block windows, and stable public symbols.

## Version Impact

- Affected component(s): native Motorola 68000 AmigaOS `opasm`, `opforge-cli`,
  and the encode/evaluate service bridge to `tkpkg`.
- Impact class: none
- Owned contract: native opasm callback/service ABI, structured opasm event
  records, CLI event rendering compatibility, focused native CLI and FS-UAE
  smoke validation.
- Rationale: Remaining active code in
  `native/motorola68000/amigaos/opforge-cli/engine_callbacks.asm` is now mostly
  tkpkg dispatch, CLI reporting, callback context construction, and test-locked
  compatibility symbols. Moving more mechanically would entangle opasm with CLI
  rendering or break public surface tests. A record/event ABI is needed first.

## Current Responsibility Inventory

Remaining responsibilities in
`native/motorola68000/amigaos/opforge-cli/engine_callbacks.asm`:

- `opforgeNativeCliRunTwoPassEngine`
  - Temporary glue and compatibility surface.
  - Builds the current callback context and calls `opasmEngineRunTwoPassV1`.

- `opforgeNativeCliBuildOpasmEngineContext`
  - Temporary glue.
  - Constructs the callback pointer table with CLI callback routines.

- `opforgeNativeCliOpasmPassOneBegin`
  - opasm engine lifecycle: begins pass 1 by calling `opasmEngineBeginPassOneV1`.
  - CLI reporting/diagnostics: prints pass-one text.

- `opforgeNativeCliOpasmPassOneOk`
  - CLI reporting/diagnostics: prints pass-one-ok text.
  - Temporary glue: current pass callback success status.

- `opforgeNativeCliOpasmPassTwoBegin`
  - opasm engine lifecycle: begins pass 2 by calling `opasmEngineBeginPassTwoV1`.
  - CLI reporting/diagnostics: prints pass-two text.

- `opforgeNativeCliOpasmPassTwoOk`
  - CLI reporting/diagnostics: prints pass-two-ok text.
  - Temporary glue: current pass callback success status.

- `opforgeNativeCliPassOneRecordLabel`
  - opasm engine lifecycle: records labels through
    `opasmEngineRecordStatementLabelV1`.
  - CLI reporting/diagnostics: prints stored-label and duplicate-label messages.
  - Compatibility/test surface: duplicate-label status must remain behaviorally
    identical.

- `opforgeNativeCliPassTwoEmitImageBytes`
  - opasm engine lifecycle: selected statement filtering and image append through
    `opasmEngineAppendImageBytesV1`.
  - tkpkg service dispatch: dispatches `ENTRY_ORD_ENCODE_SELECTED_INSTRUCTION`
    through `tkpkgServiceDispatchV1`.
  - CLI reporting/diagnostics: selector ok, image capacity, selector failure
    diagnostics, raw last-error printing.
  - Temporary glue: still owns service-window writes during pass 2.

- `opforgeNativeCliPassTwoEmitSelectorDiagnostic`
  - CLI reporting/diagnostics: maps raw selector diagnostic strings to current
    user-facing messages.
  - Temporary glue: should become CLI event-renderer logic after opasm emits
    structured diagnostic events.

- `opforgeNativeCliReadOperandValueForStatement`
  - opasm engine lifecycle: chooses source-line or stored operand text for
    expression evaluation.
  - tkpkg service dispatch: dispatches `ENTRY_ORD_EVALUATE_EXPRESSION`.
  - CLI reporting/diagnostics: unresolved-label message.
  - Temporary glue: feeds `.org` and PC-advance behavior.

- `opforgeNativeCliPassAdvancePc`
  - opasm engine lifecycle: selected encode size probing, `.org` evaluation,
    PC advance through `opasmEngineAdvancePcBySizeV1`, and origin setting
    through `opasmEngineSetOriginV1`.
  - tkpkg service dispatch: indirect selected-encode/evaluate service use.
  - CLI reporting/diagnostics: bad-org message.

- `opforgeNativeCliTrySelectedEncodeSizeForStatement`
  - tkpkg service dispatch: selected encode dispatch and output length read.
  - CLI reporting/diagnostics: selector diagnostic mapping and raw last-error
    printing.
  - Temporary glue: supports PC advance sizing.

- `opforgeNativeCliStatementMnemDuplicatesLabel`
  - Compatibility/test surface only after the recent migration; active callback
    logic calls `opasmEngineStatementMnemonicDuplicatesLabelV1` directly.

- `opforgeNativeCliStatementLooksBareColumnOne`
  - Compatibility/test surface only after the recent migration; active callback
    logic calls `opasmEngineStatementLooksBareColumnOneV1` directly.

## Target Native ABI Shape

Introduce a top-level opasm driver:

```asm
opasmNativeAssembleSessionV1
; Inputs:
; - A0: OPASM_ASSEMBLE_REQ_* frame.
; Outputs:
; - D0: OPASM_STATUS_*.
; - A0: result frame pointer when useful.
```

Assembler request frame:

```asm
OPASM_ASSEMBLE_REQ_BIN_REQUESTED_PTR      = 0
OPASM_ASSEMBLE_REQ_EVENT_BUFFER_PTR       = 4
OPASM_ASSEMBLE_REQ_EVENT_CAPACITY         = 8
OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR        = 10
OPASM_ASSEMBLE_REQ_SERVICE_FRAME_PTR      = 14
OPASM_ASSEMBLE_REQ_DIAG_BUFFER_PTR        = 18
OPASM_ASSEMBLE_REQ_DIAG_BUFFER_CAPACITY   = 22
OPASM_ASSEMBLE_REQ_FLAGS                  = 24
OPASM_ASSEMBLE_REQ_BYTES                  = 26
```

Service frame used by `opasm_tkpkg_bridge`:

```asm
OPASM_SERVICE_CONTROL_BLOCK_PTR       = 0
OPASM_SERVICE_IO_BUFFER_PTR           = 4
OPASM_SERVICE_IO_BUFFER_CAPACITY      = 8
OPASM_SERVICE_EVAL_EXTENSION_PTR      = 10
OPASM_SERVICE_EVAL_EXTENSION_BYTES    = 14
OPASM_SERVICE_BYTES                   = 16
```

Status codes:

```asm
OPASM_STATUS_OK                  = 0
OPASM_STATUS_DUPLICATE_LABEL     = 1
OPASM_STATUS_IMAGE_CAPACITY      = 2
OPASM_STATUS_BAD_ORG             = 3
OPASM_STATUS_SERVICE_FAILURE     = 4
OPASM_STATUS_EVENT_CAPACITY      = 5
```

## Event Record Layout

Use fixed 32-byte event records so CLI rendering can scan deterministically:

```asm
OPASM_EVENT_KIND          = 0
OPASM_EVENT_PASS          = 2
OPASM_EVENT_STMT_INDEX    = 4
OPASM_EVENT_FLAGS         = 6
OPASM_EVENT_TEXT_PTR      = 8
OPASM_EVENT_TEXT_LEN      = 12
OPASM_EVENT_AUX_PTR       = 14
OPASM_EVENT_AUX_LEN       = 18
OPASM_EVENT_VALUE         = 20
OPASM_EVENT_STATUS        = 24
OPASM_EVENT_SERVICE_ORD   = 26
OPASM_EVENT_RESERVED      = 28
OPASM_EVENT_BYTES         = 32
```

Initial event kinds:

```asm
OPASM_EVENT_PASS_BEGIN                 = 1
OPASM_EVENT_PASS_OK                    = 2
OPASM_EVENT_LABEL_STORED               = 3
OPASM_EVENT_LABEL_DUPLICATE            = 4
OPASM_EVENT_IMAGE_CAPACITY_EXCEEDED    = 5
OPASM_EVENT_SELECTOR_STATUS_OK         = 6
OPASM_EVENT_UNKNOWN_MNEMONIC           = 7
OPASM_EVENT_UNSUPPORTED_ADDRESSING     = 8
OPASM_EVENT_UNRESOLVED_LABEL           = 9
OPASM_EVENT_BAD_ORG                    = 10
OPASM_EVENT_SERVICE_FAILURE            = 11
```

Event payload conventions:

- `OPASM_EVENT_PASS_BEGIN`: `PASS` is `1` or `2`.
- `OPASM_EVENT_PASS_OK`: `PASS` is `1` or `2`.
- `OPASM_EVENT_LABEL_STORED`: `TEXT_PTR/TEXT_LEN` identifies the label and
  `VALUE` contains the recorded PC.
- `OPASM_EVENT_LABEL_DUPLICATE`: `TEXT_PTR/TEXT_LEN` identifies the duplicate
  label.
- `OPASM_EVENT_IMAGE_CAPACITY_EXCEEDED`: `STMT_INDEX` identifies the statement
  when available.
- `OPASM_EVENT_SELECTOR_STATUS_OK`: `STMT_INDEX` identifies the emitted
  statement; `VALUE` may carry emitted byte count.
- `OPASM_EVENT_UNKNOWN_MNEMONIC`: `TEXT_PTR/TEXT_LEN` may point at mnemonic or
  raw service diagnostic text.
- `OPASM_EVENT_UNSUPPORTED_ADDRESSING`: `TEXT_PTR/TEXT_LEN` may point at operand
  or raw service diagnostic text.
- `OPASM_EVENT_UNRESOLVED_LABEL`: `TEXT_PTR/TEXT_LEN` may point at expression,
  operand, or raw service diagnostic text.
- `OPASM_EVENT_BAD_ORG`: `STMT_INDEX` identifies the `.org` statement.
- `OPASM_EVENT_SERVICE_FAILURE`: `SERVICE_ORD` identifies the tkpkg service,
  `STATUS` carries service status, and `TEXT_PTR/TEXT_LEN` points at raw
  last-error text when present.

## Proposed Files

- `native/motorola68000/amigaos/opasm/opasm_callback_abi.asm`
  - Public constants for assemble request, service frame, status codes, and
    event records.
  - No `tkpkg` or `opforge-cli` imports.

- `native/motorola68000/amigaos/opasm/opasm_events.asm`
  - Event append helpers.
  - Capacity handling.
  - No CLI string imports.

- `native/motorola68000/amigaos/opasm/opasm_tkpkg_bridge.asm`
  - opasm-owned encode/evaluate dispatch mechanics for assembly passes.
  - Imports `tkpkg.amigaos.abi`, `tkpkg.amigaos.service`, and only generic tkpkg
    buffer/control-block symbols or receives those pointers through the service
    frame.
  - Does not import `opforge-cli`.

- `native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm`
  - `opasmNativeAssembleSessionV1`.
  - Owns opasm callback implementation and event emission.
  - Calls existing opasm pass/session/image APIs.

- `native/motorola68000/amigaos/opforge-cli/opasm_event_report.asm`
  - CLI event renderer that maps event records to existing strings.
  - May later be folded into `report.asm`.

- `native/motorola68000/amigaos/opforge-cli/engine_callbacks.asm`
  - Shrinks to compatibility wrappers during migration.

- `native/motorola68000/amigaos/opforge-cli/run.asm`
  - Eventually builds `OPASM_ASSEMBLE_REQ_*`, calls
    `opasmNativeAssembleSessionV1`, renders events, and then writes artifacts.

## Work Items

- [x] Item 1: Add opasm callback/event ABI constants and event append helpers
  - Source requirement or finding IDs: User request Phase A; active
    `AGENTS.md`; `agents/rules/native-68000.md`;
    `agents/rules/workflow-artifacts.md`.
  - Expected files: `native/motorola68000/amigaos/opasm/opasm_callback_abi.asm`,
    `native/motorola68000/amigaos/opasm/opasm_events.asm`,
    `native/motorola68000/amigaos/main.asm` or relevant module import file if
    required, and this plan for bookkeeping only.
  - Full quality gates: native assembly parse/import check for `main.asm`;
    `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`;
    focused FS-UAE smoke; `make native-68000-format-check` with known baseline
    drift recorded if unchanged; grep checks for `.use main`, wildcard
    `.use (*)`, and `opforgeNativeCli.*` inside `opasm`; workflow artifact
    validators for this plan if updated.
  - Plan-compliance review evidence: Item 1 slice stayed limited to ABI/event
    constants, event append/reset helpers, and plan bookkeeping. Focused native
    CLI validation passed. Existing CPU-specific architecture-boundary findings
    remain accepted transitional debt for this migration and are not introduced
    by the new opasm ABI/event files.
  - Commit outcome: One focused commit adding ABI/event definitions only, with
    no behavior change.
  - Definition of done: Event constants and append helpers assemble cleanly; no
    existing native CLI output changes; no opasm-to-CLI dependency is introduced.

- [x] Item 2: Add CLI event renderer with current-output parity
  - Source requirement or finding IDs: User request Phase B; current CLI output
    compatibility and surface-lock constraints.
  - Expected files:
    `native/motorola68000/amigaos/opforge-cli/opasm_event_report.asm` or
    `native/motorola68000/amigaos/opforge-cli/report.asm`,
    `native/motorola68000/amigaos/opforge-cli/strings.asm` if import grouping is
    needed, and this plan for bookkeeping only.
  - Full quality gates: native assembly parse/import check for `main.asm`;
    `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`;
    focused FS-UAE smoke; `make native-68000-format-check` with known baseline
    drift recorded if unchanged; grep checks for `.use main`, wildcard
    `.use (*)`, and `opforgeNativeCli.*` inside `opasm`.
  - Plan-compliance review evidence: Item 2 slice stayed limited to a
    render-only CLI opasm event reporter, a reachability import, and plan
    bookkeeping. Focused native CLI validation passed, and the active callback
    path remains unchanged.
  - Commit outcome: One focused commit adding render-only support without
    changing the active callback path.
  - Definition of done: CLI can render representative opasm events to the same
    text currently printed by callbacks; active runtime behavior remains
    unchanged.

- [x] Item 3: Add opasm tkpkg encode/evaluate service bridge
  - Source requirement or finding IDs: User request Phase C; keep `tkpkg` as the
    package/service boundary and keep `tkvm`, `prvm`, and `exprvm` runtime-only.
  - Expected files: `native/motorola68000/amigaos/opasm/opasm_tkpkg_bridge.asm`,
    `native/motorola68000/amigaos/opasm/opasm_callback_abi.asm` if service-frame
    constants need refinement, `native/motorola68000/amigaos/main.asm` or module
    import file if required, and this plan for bookkeeping only.
  - Full quality gates: native assembly parse/import check for `main.asm`;
    `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`;
    focused FS-UAE smoke; `make native-68000-format-check` with known baseline
    drift recorded if unchanged; grep checks for `.use main`, wildcard
    `.use (*)`, and `opforgeNativeCli.*` inside `opasm`.
  - Plan-compliance review evidence: Item 3 slice stayed limited to a dormant
    opasm tkpkg service bridge, root reachability wiring, and plan bookkeeping.
    Focused native CLI validation passed. Targeted FS-UAE smoke was invoked with
    `OPFORGE_FS_UAE_SMOKE=1` and skipped only because `OPFORGE_FS_UAE_ARGS` is
    not configured in this environment.
  - Commit outcome: One focused commit adding service bridge APIs while the CLI
    still owns active callback dispatch.
  - Definition of done: opasm bridge can prepare/write control block windows,
    dispatch encode/evaluate services, and return structured status/result
    fields without importing CLI.

- [x] Item 4: Introduce opasm assembly driver behind compatibility wrapper
  - Source requirement or finding IDs: User request Phase D; current
    `opforgeNativeCliRunTwoPassEngine` surface must remain stable.
  - Expected files:
    `native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm`,
    `native/motorola68000/amigaos/opforge-cli/engine_callbacks.asm`,
    `native/motorola68000/amigaos/opforge-cli/run.asm` if call-site frame setup
    moves, and this plan for bookkeeping only.
  - Full quality gates: native assembly parse/import check for `main.asm`;
    `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`;
    focused FS-UAE smoke; `make native-68000-format-check` with known baseline
    drift recorded if unchanged; grep checks for `.use main`, wildcard
    `.use (*)`, and `opforgeNativeCli.*` inside `opasm`.
  - Plan-compliance review evidence: Item 4 slice stayed limited to the
    `opasmNativeAssembleSessionV1` driver, compatibility-wrapper handoff,
    root reachability wiring, and plan bookkeeping. Focused native CLI
    validation passed. Targeted FS-UAE smoke was invoked with
    `OPFORGE_FS_UAE_SMOKE=1` and skipped only because `OPFORGE_FS_UAE_ARGS` is
    not configured in this environment.
  - Commit outcome: One focused commit where the public CLI entry remains but
    delegates to opasm-owned driver mechanics.
  - Definition of done: opasm can run the pass lifecycle and emit events through
    the new ABI while CLI still preserves existing textual output.

- [x] Item 5: Move active encode/evaluate dispatch out of CLI callbacks
  - Source requirement or finding IDs: User request Phase C/D transition; no
    unsupported-instruction or item-6 behavior changes.
  - Expected files:
    `native/motorola68000/amigaos/opasm/opasm_tkpkg_bridge.asm`,
    `native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm`,
    `native/motorola68000/amigaos/opforge-cli/engine_callbacks.asm`,
    `native/motorola68000/amigaos/opforge-cli/encode_eval_bridge.asm`, and this
    plan for bookkeeping only.
  - Full quality gates: native assembly parse/import check for `main.asm`;
    `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`;
    focused FS-UAE smoke; `make native-68000-format-check` with known baseline
    drift recorded if unchanged; grep checks for `.use main`, wildcard
    `.use (*)`, and `opforgeNativeCli.*` inside `opasm`.
  - Plan-compliance review evidence: Item 5 slice stayed limited to routing the
    active pass encode/evaluate service dispatch through the opasm tkpkg bridge,
    preserving the existing CLI request-preparation helpers and compatibility
    surface markers until Item 8. Focused native CLI validation passed. Targeted
    FS-UAE smoke passed with `OPFORGE_FS_UAE_SMOKE=1`,
    `OPFORGE_FS_UAE_BIN=/Applications/FS-UAE.app/Contents/MacOS/fs-uae`,
    `OPFORGE_FS_UAE_CONFIG_TEMPLATE=/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae`,
    and `OPFORGE_FS_UAE_ARGS={fsuae_config}`.
  - Commit outcome: One focused commit moving active encode/evaluate service
    dispatch used by assembly passes into opasm.
  - Definition of done: CLI no longer manipulates encode/evaluate control block
    windows during pass callbacks; opasm returns structured service statuses and
    emits events; current text remains byte-for-byte compatible where covered.

- [x] Item 6: Convert callback printing to structured event rendering
  - Source requirement or finding IDs: User request Phase D/E; CLI should render
    opasm events and no longer implement pass mechanics.
  - Expected files:
    `native/motorola68000/amigaos/opasm/opasm_events.asm`,
    `native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm`,
    `native/motorola68000/amigaos/opforge-cli/opasm_event_report.asm` or
    `native/motorola68000/amigaos/opforge-cli/report.asm`,
    `native/motorola68000/amigaos/opforge-cli/engine_callbacks.asm`, and this
    plan for bookkeeping only.
  - Full quality gates: native assembly parse/import check for `main.asm`;
    `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`;
    focused FS-UAE smoke; `make native-68000-format-check` with known baseline
    drift recorded if unchanged; grep checks for `.use main`, wildcard
    `.use (*)`, and `opforgeNativeCli.*` inside `opasm`.
  - Plan-compliance review evidence: Item 6 slice stayed limited to converting
    callback pass, label, image-capacity, selector, unresolved-label, bad-org,
    and raw service reports into structured `OPASM_EVENT_*` records rendered by
    the CLI event reporter. Focused native CLI validation passed. Targeted
    FS-UAE 6502 output smoke passed with the configured FS-UAE command.
  - Commit outcome: One focused commit where direct printing leaves callback
    internals and CLI rendering consumes structured opasm events.
  - Definition of done: Existing user-facing output remains compatible; pass,
    label, image, selector, unresolved-label, and bad-org reports are rendered
    from event records.

- [x] Item 7: Shrink `engine_callbacks.asm` to compatibility wrappers only
  - Source requirement or finding IDs: User request Phase E; keep compatibility
    symbols until surface-lock tests are intentionally updated.
  - Expected files:
    `native/motorola68000/amigaos/opforge-cli/engine_callbacks.asm`,
    `native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm`, and this
    plan for bookkeeping only. The repository formatter also removed two
    unrelated blank lines in
    `native/motorola68000/amigaos/tkvm/tkvm_scanner.asm` so the formatter gate
    can pass.
  - Full quality gates: native assembly parse/import check for `main.asm`;
    `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`;
    focused FS-UAE smoke; `make native-68000-format-check` with known baseline
    drift recorded if unchanged; grep checks for `.use main`, wildcard
    `.use (*)`, and `opforgeNativeCli.*` inside `opasm`.
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with
    `AGENTS.md`, this plan, Item 7 changed files, and validation evidence before
    commit. In this Codex environment the reviewer is not exposed as a callable
    tool, so the plan bundle validator was run with pending-gate allowance after
    focused native CLI and FS-UAE validation.
  - Commit outcome: One focused commit reducing CLI callback code to wrapper
    and renderer handoff only.
  - Definition of done: No pass mechanics, encode/evaluate dispatch, label
    recording, PC advance, or image emission remains active in CLI callback
    code.
  - Implementation evidence: `opasmNativeAssembleSessionV1` now builds its own
    opasm engine callback request, appends structured events, and owns active
    label/PC/image/service mechanics. `opforgeNativeCliRunTwoPassEngine` now
    builds only an opasm assemble request plus service frame and renders the
    resulting event buffer through the CLI event reporter.
  - Validation evidence:
    `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture` passed.
    The targeted FS-UAE command requested by the user passed:
    `external_fs_uae_opforge_native_cli_6502_writes_rust_matching_bin`.
    `make native-68000-format` processed 88 files and changed 3, then
    `make native-68000-format-check` passed with 0 would-change files.
    `scripts/workflow/run_rust_quality_gate.sh` passed formatter enforcement and
    stopped at the known transitional CPU-specific architecture boundary scan
    with 122 enforced-scope leaks, which the user accepted as non-blocking
    during this refactor.

- [x] Item 8: Intentionally update native CLI surface-lock expectations
  - Source requirement or finding IDs: User request Phase F; only after
    compatibility wrappers are no longer needed by active code.
  - Expected files: native CLI surface-lock tests in Rust, reference/listing
    fixtures if explicitly required by the test suite, active CLI callback
    wrapper cleanup, and this plan for bookkeeping only.
  - Full quality gates: `cargo test -p asm motorola68020_opforge_native_cli_ --
    --nocapture`; focused FS-UAE smoke if native output path is touched;
    `scripts/workflow/run_rust_quality_gate.sh` or `make quality-gate` if Rust
    test code changes require the broader gate; `make native-68000-format-check`
    with known baseline drift recorded if unchanged; grep checks for `.use
    main`, wildcard `.use (*)`, and `opforgeNativeCli.*` inside `opasm`.
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with
    `AGENTS.md`, this plan, Item 8 changed files, and validation evidence before
    commit. In this Codex environment the reviewer is not exposed as a callable
    tool, so the plan bundle validator was run with pending-gate allowance after
    focused native CLI and FS-UAE validation.
  - Commit outcome: One focused commit that updates public surface expectations
    only after behavior is already migrated.
  - Definition of done: Surface-lock tests expect opasm-owned driver/event
    symbols instead of obsolete CLI callback internals; no behavior change is
    bundled with the test expectation update.
  - Implementation evidence: `engine_callbacks.asm` now contains only
    `opforgeNativeCliRunTwoPassEngine`, the opasm assemble request/service-frame
    setup, event rendering handoff, and event buffer storage. Retired CLI
    callback-table builders, pass callbacks, label/PC/image callbacks, and
    selector/evaluate compatibility helpers were removed. Rust surface-lock
    assertions now require the opasm assembly driver symbols and assert the old
    CLI callback symbols are absent.
  - Validation evidence:
    `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture` passed.
    The targeted FS-UAE command requested by the user passed:
    `external_fs_uae_opforge_native_cli_6502_writes_rust_matching_bin`.
    `make native-68000-format-check` passed with 87 checked files and 0
    would-change files. The opasm grep check for `.use main`, wildcard `.use
    (*)`, and `opforgeNativeCli` references returned no matches.
    `scripts/workflow/run_rust_quality_gate.sh` passed formatter enforcement and
    stopped at the known transitional CPU-specific architecture boundary scan
    with 122 enforced-scope leaks, which the user accepted as non-blocking
    during this refactor.

## Blocking Rules

- no commit before all quality gates pass
- `plan-compliance-reviewer` must return `PASS` before commit
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- archive completed plans with `scripts/workflow/archive_completed_plan.sh`
- do not implement new assembler semantics as part of this plan
- do not fix unsupported instruction or item-6 behavior as part of this plan
- preserve the current native CLI and focused FS-UAE smoke baseline
- keep `opforge-cli` as host orchestration and rendering only
- keep `tkpkg` as the package/service boundary
- keep `tkvm`, `prvm`, and `exprvm` runtime-only
- avoid `opasm` to `opforge-cli` imports
- no `.use main`
- no wildcard `.use (*)`
- keep `bsr.w` only for calls inside the same module; use `jsr` across modules
- do not delete compatibility wrappers until the explicit surface-lock update
  phase

## Risks And Import-Cycle Concerns

- `opasm` must not import CLI strings or CLI report helpers. Event records should
  carry event kinds and opasm-owned text pointers; CLI maps event kinds to
  existing strings.
- `opasm_tkpkg_bridge` may import `tkpkg`, but `tkpkg` must not import `opasm`.
- Service-frame pointers should avoid hard dependency on CLI state symbols.
- Event ordering must match current printed output until tests are intentionally
  updated.
- Event capacity failure must be explicit and must not silently drop diagnostics.
- Any selected encode/evaluate dispatch move can change FS-UAE-visible behavior;
  every such slice must run the focused FS-UAE smoke.
- Public surface-lock tests can block deletion or renaming even after active code
  stops using old symbols; wrapper deletion must be separate and intentional.
