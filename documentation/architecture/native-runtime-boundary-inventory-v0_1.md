# Native Runtime Boundary Inventory v0.1

## Scope and method

This is the Item 5.2 Level B inventory for the eight modules named by
NR-001 through NR-008.  It records static ownership evidence only: module
imports, `.pub` entry surfaces, `.block` routine groups, module-local state
sections, and diagnostic/status/event paths.  It does not claim semantic
parity, authorize an extraction, or alter a production path.

The companion validator is the machine-readable complete manifest.  It
derives and prints **every** `.block` routine, `.use` import, `.section` state
block, and diagnostic/status/event source line with `--report`; a source hash
and counts for every audited source fail closed on drift.  Therefore an edit
cannot silently make this human ownership record incomplete: it must refresh
the manifest and its decision in a later scoped item.  The routine-group
descriptions below assign each derived routine to its module's declared
responsibility group; they do not declare every private routine a public API.

## Dependency direction

```text
CLI/session frontend -> assembly driver -> engine callback API
                                    -> flow owners / text-encoding owner
                                    -> tkpkg bridge -> tkpkg service facade
tkpkg service facade -> pipeline, tokenizer VM, PRVM line router,
                        expression bridge, package loader
pipeline -> package hierarchy / CPU-family-dialect locators
expression bridge -> expression VM runtime
```

The desired direction is toward narrow service or runtime contracts.  The
current `tkpkg.amigaos.service -> opasm.amigaos.engine` import is a direct
cross-subsystem dependency and is a transitional finding for Items 5.5.1,
5.7, and 5.7.1; it must become a named neutral runtime-context adapter rather
than a package runtime addressing engine-owned mutable tables.

## Audited modules

### `opasm.amigaos.assembly_driver` (NR-006, mandatory decomposition)

- Source: `native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm`.
- Public entry: `assembleSessionV1`; it builds the engine callback context and
  runs the two-pass session.
- Imports/outbound dependencies: callback ABI, compile values, engine, events,
  conditional/navigation/repetition/scope/struct flow modules, text encoding,
  tkpkg bridge, and approved debug contracts/events.
- Mutable state: module-local pass/session request pointers, flow/repetition
  scratch, layout region/section tables, and text scratch/output state.
- Routine responsibility groups: pass callback orchestration; directive and
  mnemonic routing; structural-flow matching scans; operand/evaluation request
  construction; selector/encoding adaptation; data/text sizing and emission;
  layout/region/section/place handling; event projection.
- Inbound users: the CLI engine-callback adapter imports this driver; the
  driver is the session orchestration boundary, not a package or CPU owner.
- Decision: orchestration stays here.  The semantic groups are mandatory
  future owners: directive router (5.8), structural flow (5.8.1), operand
  evaluation (5.9), selector adaptation (5.9.1), data (5.9.2), text (5.9.3),
  and layout (5.9.4).  Repeated directive/scanner comparisons are candidates
  for one bounded shared utility only after the router extraction proves need.

### `tkpkg.amigaos.service` (NR-002/003/004, mandatory decomposition)

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm`.
- Public entries: `bootstrapV1` and `dispatchV1`.
- Imports/outbound dependencies: tkpkg ABI/buffers, dedicated request-lifecycle,
  status-projection, parser-adapter, expression-service, and selection-service
  owners; engine, expression bridge, package loader, pipeline, and tokenizer VM.
  The expression service reaches the temporary engine-state boundary only through
  its named `tkpkg.amigaos.expression_context` adapter.
- Mutable state: request/control-block pointers, output and last-error buffers,
  and service result fields.
- Routine responsibility groups: bootstrap/request validation; status and
  diagnostic projection; parser route adaptation; transitional expression
  contract validation; selected-envelope encoding/output construction; and the
  retained package contract/locator helpers.
- Inbound users: the opasm tkpkg bridge is the principal facade caller.
- Decision: retain only ABI dispatch, output projection, and last-error entry
  in the facade. Item 5.4 extracted status/error and output-window
  implementation to `tkpkg.amigaos.service_status`; Item 5.4.1 extracts
  bootstrap, header validation, and request bookkeeping to
  `tkpkg.amigaos.service_request`. The facade keeps only explicit compatibility
  delegates pending caller migration. Item 5.5 extracts the fixed PRVM route
  frame adapter to `tkpkg.amigaos.parse_service`; Item 5.5.1 moves expression
  envelope preparation and bridge execution to
  `tkpkg.amigaos.expression_service` and places the temporary engine read behind
  `tkpkg.amigaos.expression_context`. The facade retains package-contract
  validation only until the neutral-context migration. Item 5.6 moves selection
  decoding and candidate traversal to `tkpkg.amigaos.selection_service`; Item
  5.6.1 moves the unchanged operand-plan runtime to
  `tkpkg.amigaos.operand_runtime`, and Item 5.6.2 moves package-table encoding
  to `tkpkg.amigaos.encode_service`. This is
  an ownership-only file split: it does not add, broaden, or
  validate support for any CPU, family, dialect, plan tag, or instruction. The
  repeated package-string/locator helpers overlap pipeline-style utilities and
  require an ownership decision before consolidation; no unproven helper merge
  is authorized here.

### `tkpkg.amigaos.selection_service` (NR-004, Items 5.6–5.6.1 ownership split)

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_selection_service.asm`.
- Public entries: `selectInstructionV1`, `buildSelectedEnvelopeV1`, and
  `noOutputErrorV1`.
- Imports/outbound dependencies: tkpkg ABI/buffers plus the existing engine and
  expression bridge transition boundaries.
- Mutable state: selected request envelope and candidate traversal cursor; the
  unchanged operand scratch state is shared through the internal selection-state
  module.
- Routine responsibility groups: selected-request decoding; package MSEL
  traversal; candidate construction; selected-output diagnostic selection.
- Decision: this module delegates existing plan interpretation to
  `tkpkg.amigaos.operand_runtime`. Item 5.7.2 will replace remaining direct
  engine reads with the neutral runtime context. Neither item expands CPU
  support or changes package semantics.

### `tkpkg.amigaos.operand_runtime` (NR-004, Item 5.6.1 ownership split)

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_operand_runtime.asm`.
- Public entry: `tkpkgMselTryBuildCandidateV1`.
- Imports/outbound dependencies: tkpkg buffers and private selection state, plus
  the existing engine and expression bridge transition boundaries.
- Mutable state: reads and writes the preserved selection-state scratch layout;
  it does not own package selection or selected-output diagnostics.
- Routine responsibility groups: unchanged plan-tag dispatch, operand-span
  normalization, expression evaluation, and candidate-envelope construction.
- Decision: this is a file-boundary extraction only. Existing plan tags and
  emitted bytes are retained exactly; no CPU, family, dialect, or instruction
  support is added or generalized.

### `tkpkg.amigaos.encode_service` (NR-004, Item 5.6.2 ownership split)

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_encode_service.asm`.
- Public entries: `encodeInstructionV1` and `encodeSelectedInstructionV1`.
- Imports/outbound dependencies: tkpkg ABI/buffers and the existing selection
  service boundary.
- Mutable state: writes the same existing package-service output buffer; it does
  not own pipeline selection, package loading, or status projection.
- Routine responsibility groups: selected-envelope encoding, package-table
  lookup, encoding-program execution, and encoded-output construction.
- Decision: this is an ownership-only file split. Package data, selector
  ordering, plan tags, status/diagnostic paths, and emitted bytes remain
  unchanged; no CPU, family, dialect, or instruction support is added.

### `tkpkg.amigaos.runtime_context` (NR-005, Item 5.7 ownership split)

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_runtime_context.asm`.
- Public entries: `getAbiVersionV1`, `getPassV1`, `getAddressV1`,
  `lookupSymbolV1`, `reportDiagnosticV1`, and `getLastDiagnosticV1`.
- Imports/outbound dependencies: only the engine-context adapter.
- Mutable state: one private neutral diagnostic record; it is not an engine,
  CLI, or package-service buffer.
- Routine responsibility groups: versioned read-only context projection and
  bounded diagnostic handoff.
- Decision: this is a file-boundary split that establishes the future consumer
  contract. It neither migrates a production consumer nor changes CPU, family,
  dialect, instruction, selector, plan-tag, or encoding support.

### `tkpkg.amigaos.engine_context_adapter` (NR-005, Item 5.7 ownership split)

- Source:
  `native/motorola68000/amigaos/tkpkg/tkpkg_engine_context_adapter.asm`.
- Public entries: `getPassV1`, `getAddressV1`, and `lookupSymbolV1`.
- Imports/outbound dependencies: documented engine getter APIs only.
- Mutable state: none; it translates engine-owned label/pass/address state to
  the runtime-context ABI and never exposes engine table layout.
- Routine responsibility groups: the sole transitional engine access point for
  future tkpkg context consumers.
- Decision: this is a file-boundary split only. Items 5.7.1 and 5.7.2 migrate
  existing consumers; this item adds no CPU or package semantics.

### `opasm.amigaos.engine` (NR-001, conditional decomposition)

- Source: `native/motorola68000/amigaos/opasm/opasm_engine.asm`.
- Public surface: session initialization, source/statement collection,
  callback-context construction, pass execution, labels, PC/image access, and
  selector/expression request preparation APIs.
- Imports/outbound dependencies: event projection only.
- Mutable state: assembly session allocation, source/statement records, label
  table, pass/PC/image state, callback context, and diagnostic/event state.
- Routine responsibility groups: session collection and lifecycle; two-pass
  runner; label/image/PC ownership; callback API; request preparation.
- Inbound users: CLI session/source/report components, assembly driver, tkpkg
  service, and test/debug harnesses import the engine API.
- Decision: retain cohesively pending Item 5.11.  It is the documented owner
  of statement/pass/image state; only a proved mixed responsibility or a
  prohibited dependency direction permits decomposition.

### `tkpkg.amigaos.tokenizer_vm` (NR-005, retain cohesive)

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_tokenizer_vm.asm`.
- Public entry: `tkpkgTokenizerVmTokenizeLineV1`.
- Imports/outbound dependencies: tkpkg ABI/buffers and TKVM runtime/control/
  state.
- Mutable state: tokenizer request/result and diagnostic/output rendering
  scratch.
- Routine responsibility groups: request/program parsing, TKVM invocation,
  result validation, diagnostic construction, and bounded token rendering.
- Inbound users: tkpkg service.
- Decision: retain cohesive.  This is one package-runtime adapter; no direct
  opasm state import or semantic ownership split was found.

### `opcore.amigaos.expr_bridge` (NR-008, explicit later decision)

- Source: `native/motorola68000/amigaos/opcore/opcore_expr_bridge.asm`.
- Public entries: `opcoreExprEvalOperandV1` and `opcoreExvmEvalOperandV1`.
- Imports/outbound dependencies: expression VM runtime.
- Mutable state: expression bytecode/program buffer, parser cursor, literal
  scratch, and evaluation result state.
- Routine responsibility groups: expression program selection, expression
  parsing/bytecode compilation, symbol/literal handling, and VM execution.
- Inbound users: tkpkg service and expression-oriented native paths.
- Decision: Item 5.10 must decide retain, split, or reduce it to a narrow
  adapter based on ownership evidence.  Its source does not itself import
  opasm state, but its callers currently supply opasm-derived context.

### `prvm.amigaos.runtime` (NR-005, retain cohesive)

- Source: `native/motorola68000/amigaos/prvm/prvm_runtime.asm`.
- Public entry: `prvmRun68000`.
- Imports/outbound dependencies: PRVM ABI/state/bytecode support and the
  package line-router boundary.
- Mutable state: VM token cursor, checkpoint stack, result records, emitted
  statement fields, and expression resume state.
- Routine responsibility groups: bytecode execution, token access,
  checkpointing, statement-result construction, and expression suspension/
  resume.
- Inbound users: tkpkg service through the line router.
- Decision: retain cohesive.  Its parser-VM state is internally coupled and
  does not own CLI orchestration, package selection, or opasm tables.

### `tkpkg.amigaos.pipeline` (NR-004, conditional decomposition)

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm`.
- Public entry: `tkpkgPipelineSetActiveV1`.
- Imports/outbound dependencies: tkpkg ABI/buffers and token policy.
- Mutable state: active package selection and CPU/family/dialect/tokenizer/
  parser locator buffers.
- Routine responsibility groups: request parsing, package hierarchy lookup,
  CPU/family/dialect selection, tokenizer/parser locator resolution, and
  selection commit.
- Inbound users: tkpkg service and package-facing setup paths.
- Decision: retain cohesive pending Item 5.11.  CPU/family/dialect terms are
  package data resolution responsibilities here, not generic opasm behavior.
  Locator/string helpers are a duplicated-helper audit finding shared with
  service, not permission to move package semantics into the facade.

### `opasm.amigaos.flow_text_encoding` (NR-007, retain cohesive)

- Source: `native/motorola68000/amigaos/opasm/opasm_flow_text_encoding.asm`.
- Public entries: `resetStateV1`, `routeDirectiveV1`, `encodeBytesV1`, custom
  selection, and CDEF/TDEF/EDEF definition entries.
- Imports/outbound dependencies: none; it is session-local flow state.
- Mutable state: active encoding, custom definition table/name/map, definition
  cursor, and escaping scratch.
- Routine responsibility groups: encoding lifecycle, directive routing,
  custom-definition parsing, character mapping, escaping, and byte emission.
- Inbound users: assembly driver.
- Decision: retain cohesive as the text-encoding domain owner.  Item 5.9.3
  moves driver-side text sizing/emission adaptation to this domain boundary;
  it does not split or add text semantics here.

## Cross-cutting findings and safe future landing points

- Orchestration versus semantics: driver session callbacks and service ABI
  dispatch are orchestration; package selection/encoding, expression parsing,
  flow scans, text encoding, and layout are semantic owners.
- Direct cross-subsystem state: the service imports the engine today. Item 5.7
  adds a neutral runtime-context ABI and the sole transitional engine adapter
  for pass, address, symbol lookup/stability, and diagnostics; later migration
  removes direct engine mutable-table access. This is only a large-file split,
  not CPU-support work.
- Diagnostics/output: service owns ABI-facing status/last-error projection;
  engine owns event/session state; driver only projects events through the
  approved event boundary.
- Segment and statement landing points: neither feature is activated by this
  plan.  A later segment capture route belongs above the decomposed directive
  router and below CLI source staging; a later statement route belongs at the
  engine statement-record/callback boundary.  Neither may be implemented in
  driver string chains, package runtimes, or macro transaction state.
- No routine is moved by this document.  Each mandatory or conditional target
  has a stated retain/decomposition decision before Item 5.3 contracts and
  subsequent ownership-only commits begin.
