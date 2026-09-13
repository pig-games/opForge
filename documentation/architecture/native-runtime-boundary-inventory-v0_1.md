# Native runtime boundary inventory

This retained technical inventory describes module responsibilities, entrypoints,
state and dependencies. It is not a future decomposition plan or a requirement to
preserve existing modules indefinitely. Historical change narratives and extraction
decisions are in Git history. Review actual source when using a description for
new work; the source checks do not prove every prose statement current.

The [inventory checker](../../scripts/workflow/check_native_runtime_boundary_inventory.py)
records source hashes and counts and derives `.block` routines, `.use` imports,
`.section` state and diagnostic/status/event lines. Its `--report` option prints
that structural inventory. Relevant source changes require refreshing the checked
inventory after reviewing their impact, not creating a plan item or approval receipt.
This is structural evidence, not executed native parity.

Use the [boundary contract](native-runtime-boundary-contract-v0_1.md) for existing
ownership constraints and the [repository workflow](../workflow/README.md) for
scope, validation and completion. No future feature or migration is scheduled here.

## Modules

### `tkpkg.amigaos.state_service`

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_state_service.asm`.
- Public entries: `initializeActiveV1`, `resetActiveV1`, `applyDirectiveV1`,
  `getFlagV1`, and `requirementAllowsV1`.
- Imports/outbound dependencies: `tkpkg.amigaos.buffers`; the default-off
  `debug.amigaos.runtime_profile` observer when runtime counters are enabled.
- Mutable state: the selected package-program cursor and directive-table cursor;
  bounded active key pointers, lengths, values, profile index, and program bounds
  live in the shared package buffers.
- Routine responsibility groups: bounded STVM decoding; scoped-owner matching;
  active-profile default materialization; case-insensitive directive and argument
  matching; transactional profile-mask validation; opaque key lookup; and generic
  `key=v1+v2?diagnostic` requirement evaluation.
- Inbound users: pipeline selection initializes the service, the assembly driver
  resets/applies state per pass, runtime context projects opaque flags, and the
  selector evaluates package-owned requirements.

### `opasm.amigaos.assembly_driver`

- Source: `native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm`.
- Public entry: `assembleSessionV1`; it builds the engine callback context and
  runs the two-pass session.
- Imports/outbound dependencies: callback ABI, compile values, directive router,
  numeric-data owner, engine, events, conditional/navigation/repetition/scope/struct
  flow modules, text encoding, tkpkg bridge, the architecture-neutral tkpkg ABI
  and shared pipeline buffers, the CLI-owned imported-label resolver callback,
  the default-off passive symbol/expression observer, and approved debug
  contracts/events.
- Mutable state: module-local pass/session request pointers, flow/repetition
  scratch, text scratch/output state, and one bounded copy of the request-selected
  package CPU id used to restore the same initial pipeline before each pass.
  Layout region/section/place storage is owned by `opasm.amigaos.layout`.
- Routine responsibility groups: pass callback orchestration; router-result
  dispatch using retained parser statement kind, with unresolved dot statements
  rejected before instruction selection; structural-flow state
  transitions and explicit `.case` evaluation
  callback; scoped-struct repeat-label qualification callback; operand/evaluation request
  construction, including delegation of imported-label lookup without owning
  module visibility; selector/encoding adaptation; data/text sizing and emission;
  arbitrary package-returned instruction-size advancement; generic package
  pipeline restoration at pass start and replay of source `.cpu` transitions
  through `SET_PIPELINE`; complete stored directive-operand delegation; remaining
  layout/region/section/place/pack dispatch, including bounded projection of
  Rust Hunk section-memory aliases into the layout owner; event projection.
- Inbound users: the CLI engine-callback adapter imports this driver; the
  driver is the session orchestration boundary, not a package or CPU owner.
- Reservation handling uses the driver consumers
  `prepareResOperandsForStatement`, `readResUnitForStatement`, and
  `evaluateResOperandSlice`. The driver checks the unit-times-count extent and
  inclusive last address, then passes the typed result to engine-owned RES
  storage; no per-statement table is added.

### `opasm.amigaos.directive_router`

- Source: `native/motorola68000/amigaos/opasm/opasm_directive_router.asm`.
- Public entry: `classifyV1`; it maps existing non-structural directive text to
  a numeric route code.
- Imports/outbound dependencies: the opasm engine only, for the existing
  session-pass and current-PC callbacks used by section transitions.
- Mutable state: none.
- Routine responsibility groups: case-insensitive bounded directive comparison,
  aliases for existing data directives, and numeric routing for layout-owned
  `.pack` handling.
- Inbound users: the assembly driver, which retains all callback orchestration,
  traversal, and handler execution.

### `opasm.amigaos.operand_eval`

- Source: `native/motorola68000/amigaos/opasm/opasm_operand_eval.asm`.
- Public entries: selected-instruction request construction, textual expression
  request construction, their evaluation-extension adapters, and bounded
  materialization of imported aliases supplied through the callback ABI. Directive
  requests retain those snapshots but omit instruction-shape inference.
- Imports/outbound dependencies: callback ABI, engine request builders, and the
  flow-scope owner's bounded active-label alias query.
- Mutable state: a bounded evaluation-only snapshot of local, imported, and
  qualified/global label names and values;
  the driver still supplies its service frame and owns request-length state,
  dispatch, diagnostics, and fallback policy.

### `opasm.amigaos.directive_data`

- Source: `native/motorola68000/amigaos/opasm/opasm_directive_data.asm`.
- Public entries: `sizeNumericDirectiveV1` and `emitNumericDirectiveV1`.
- Imports/outbound dependencies: engine image append and runtime-context data
  byte order; the driver supplies its
  existing comma-count and statement-aware operand-resolution callbacks.
- Mutable state: per-session callback pointers, unit-width/byte-order scratch, and a
  four-byte packing buffer.
- Routine responsibility groups: numeric list sizing, byte range validation,
  package-selected byte-order packing, and image append. Missing execution
  properties fail explicitly before emission.

### `opasm.amigaos.directive_text`

- Source: `native/motorola68000/amigaos/opasm/opasm_directive_text.asm`.
- Public entries: `sizeTextDirectiveV1` and `emitTextDirectiveV1`.

### `opasm.amigaos.layout`

- Source: `native/motorola68000/amigaos/opasm/opasm_layout.asm`.
- Public entries: region/section/place state transitions, bounded layout-name
  request APIs, `alignCursorV1`, `alignPadV1`,
  `markOutputFixupBytesNormalizedV1`, and
  `getOutputFixupBytesNormalizedV1`.
- Imports/outbound dependencies: the architecture-neutral engine query API and,
  in observer builds, the default-off platform profiler.
- Mutable state: region/section/place counters, names, bounds, cursors,
  alignment values, placement indices, Hunk memory attributes, and scratch
  storage; retained output fixups also own one bounded normalization flag per
  existing 256-entry slot.
- Routine responsibility groups: overflow-safe positive alignment,
  power-of-two padding arithmetic, bounded layout-name copy/comparison,
  region/section/place validation and transitions, including sequential `.pack`
  placement through the same transition owner, word/long table-index
  calculation, and bounded fixup normalization-state updates and queries. The
  driver retains statement tokenization, callback dispatch, and engine/image
  projection only, including the read-only `getListingSectionNameV1`
  projection over the existing statement section indices and final active
  section; it adds no layout state and does not change semantic ownership.

### `tkpkg.amigaos.service`

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm`.
- Public entries: `bootstrapV1` and `dispatchV1`.
- Imports/outbound dependencies: tkpkg ABI/buffers, dedicated request-lifecycle,
  status-projection, parser-adapter, expression-service, and selection-service
  owners; operand-record service, engine, expression bridge, package loader,
  pipeline, and tokenizer VM; plus the default-off runtime observer.
  The expression service now reaches pass/finalization state only through the
  neutral `tkpkg.amigaos.runtime_context` façade.
- Mutable state: request/control-block pointers, output and last-error buffers,
  and service result fields.
- Routine responsibility groups: bootstrap/request validation; status and
  diagnostic projection; parser route adaptation; transitional expression
  contract validation; selected-envelope encoding/output construction; and the
  retained package contract/locator helpers.
- Inbound users: the opasm tkpkg bridge is the principal facade caller.

### `tkpkg.amigaos.selection_service`

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_selection_service.asm`.
- Public entries: `selectInstructionV1`, `buildSelectedEnvelopeV1`,
  `noOutputErrorV1`, `tkpkgProjectBoundedRegisterV1`, and experimental
  `executeNumericValueV1` (already-bound VALP bytes, with caller registers preserved).
- Imports/outbound dependencies: tkpkg ABI/buffers, operand runtime, neutral
  runtime context, the expression bridge transition boundary, and the
  default-off runtime observer.
- Mutable state: selected request envelope and candidate traversal cursor; the
  unchanged operand scratch state is shared through the internal selection-state
  module. One bounded deferred-rejection text buffer is used so traversal
  can retain the highest-priority diagnostic while preserving complete owner
  search.
- Routine responsibility groups: selected-request decoding; package MSEL and
  CSEM-owner traversal; CPU-neutral CMSE-v7 scalar input projection; scoped
  RENC/VALP program lookup and execution; neutral register-list mask,
  duplicate, and distinct-register detection; direct and indirect call-argument
  register projection; neutral tuple identity-scale projection; candidate
  construction; package-declared named-register comparison and signed
  out-of-range rejection projection; dialect-CPU-family
  rejection precedence; selected-output diagnostic selection; and standard
  scalar capture rendering; package-declared bounded-register projection; and
  propagation of engine-owned label target-reference metadata into fixups.

### `tkpkg.amigaos.operand_runtime`

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_operand_runtime.asm`.
- Public entry: `tkpkgMselTryBuildCandidateV1`.
- Imports/outbound dependencies: tkpkg buffers, private selection state, neutral
  runtime context, and the expression bridge transition boundary.
- Mutable state: reads and writes the preserved selection-state scratch layout;
  it does not own package selection or selected-output diagnostics.
- Routine responsibility groups: plan-tag dispatch, operand-span normalization,
  top-level neutral operand-list splitting, expression evaluation, and
  candidate-envelope construction.

### `tkpkg.amigaos.encode_service`

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_encode_service.asm`.
- Public entries: `encodeInstructionV1` and `encodeSelectedInstructionV1`, plus
  experimental `executeNumericTableV1` and `executeNumericSemanticV1` over bound
  canonical programs and fresh numeric operands. These wrappers reuse the existing
  interpreters and output buffer without name lookup or additional state.
- Imports/outbound dependencies: tkpkg ABI/buffers, private selection state,
  the existing selection-service boundary, and the generic compact-table
  boundary and numeric semantic bindings; plus the default-off runtime observer.
- Mutable state: writes the same existing package-service output buffer; it does
  not own pipeline selection, package loading, or status projection.
- Routine responsibility groups: selected-envelope encoding, legacy
  package-table lookup, neutral CSEM owner/program lookup, direct CSEM-v2
  Literal/Scalar/Fields execution with bounds/overlap/endianness validation,
  compact fixed-row delegation, and encoded-output construction.

### `tkpkg.amigaos.semantic_bindings`

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_semantic_bindings.asm`.
- Public entries: `reset`, `find`, `store`; no outbound dependencies.
- Mutable state: 64 raw 12-byte descriptors plus count/alignment. Each descriptor
  holds a numeric CMSE name ID, program version, length and package pointer.
- Responsibility: bounded reuse of validated, owner-selected program metadata;
  full capacity falls back to ordinary resolution. No operand values, emitted
  bytes or source statements are retained.
- Inbound users: encoding resolves misses; package loading and every successful
  pipeline commit invalidate all descriptors before subsequent execution.

### `tkpkg.amigaos.compact_table`

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_compact_table.asm`.
- Public entries: `find` and `bind`.
- Imports/outbound dependencies: tkpkg ABI/buffers, the existing neutral
  scoped-owner/string helpers in the selection service, the prepared compact
  table boundary, and the default-off runtime observer through its telemetry
  macro include.
- Mutable state: the active dialect/CPU/family owner indices. `bind` publishes
  these indices only after pipeline selection succeeds; failed selection leaves
  the previous binding intact. There is no lookup memo.
- Routine responsibility groups: lookup against the immutable prepared CTBL,
  bounded reconstruction of prefix-compressed strings, scope-order selection
  among matching rows and direct prepared program lookup.

### `tkpkg.amigaos.compact_prepare`

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_compact_prepare.asm`.
- Public entries: `prepare` and `reset`.
- Imports/outbound dependencies: tkpkg buffers, the neutral chunk locator in
  the selection service, and the default-off runtime observer through its
  telemetry macro include.
- Mutable state: the validated CTBL bounds and counts plus an allocated program
  pointer directory. `reset` invalidates and frees that directory; package reload
  prepares a replacement, and package shutdown resets it.
- Routine responsibility groups: CTBL structural validation before publication,
  owner/string/program/row traversal, strict row-order and index validation,
  exact chunk-consumption checks, program-directory allocation, and transactional
  publication or cleanup on failure. Native limits remain 16-bit program lengths
  and row counts, and the existing string scratch capacity. UTF-8 and duplicate
  owner/string/program-payload validation remain gaps against Rust; the existing
  locator also treats a zero-length CTBL as absent.

### `tkpkg.amigaos.operand_record_service`

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_operand_record_service.asm`.
- Public entry: `executeRequestV1`; it parses one bounded neutral request,
  resolves an opaque CPRD id by dialect/CPU/family precedence, and executes one
  exact OPRD schema-v1 base program or schema-v2 nested-address program.
- Imports/outbound dependencies: the tkpkg ABI and shared buffers; plus the
  default-off runtime observer.
- Mutable state: bounded request cursors, active-owner indices, selected-program
  metadata, and the dedicated neutral result buffer (24-byte v1 or 40-byte v2).
- Routine responsibility groups: little-endian bounded CPRD reading; complete
  UTF-8, duplicate-owner/duplicate-id, and v1-v3 program-set validation before scoped
  owner/id matching; exact program-shape and END validation; request/result
  non-overlap enforcement; neutral register,
  indirect/update, displacement/base, indexed/width/scale, absolute, and
  immediate result materialization; nested base, optional base
  displacement, optional index, indirection, and optional outer displacement
  materialization; missing-input and malformed-data rejection.

### `tkpkg.amigaos.runtime_context`

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_runtime_context.asm`.
- Public entries: `getAbiVersionV1`, `getPassV1`, `getAddressV1`,
  `lookupSymbolV1`, `isSymbolTargetReferenceV1`,
  `getSymbolStabilityTableV1`, `getSymbolTableSnapshotV1`,
  `reportDiagnosticV1`, `getLastDiagnosticV1`, `getCpuWordSizeBytesV1`, and
  `getCpuMaxProgramAddressV1`, and `getCpuDataByteOrderV1`.
- Imports/outbound dependencies: engine-context adapter, state service and
  package-owned buffers for the selected CPU execution-property cache.
- Mutable state: private neutral diagnostic, symbol-stability, and bounded
  copied symbol-table records; none is engine, CLI, or package-service storage.
- Routine responsibility groups: versioned read-only context projection,
  bounded diagnostic handoff, bounded stability snapshot materialization, and
  read-only projection of engine-owned label target-reference metadata and
  explicit-presence access to selected package CPU word size/address bounds/data byte order.

### `tkpkg.amigaos.engine_context_adapter`

- Source:
  `native/motorola68000/amigaos/tkpkg/tkpkg_engine_context_adapter.asm`.
- Public entries: `getPassV1`, `getAddressV1`, `lookupSymbolV1`,
  `isSymbolTargetReferenceV1`, `isSymbolFinalV1`, `getSymbolCountV1`,
  `getSymbolNameV1`, and `getSymbolValueV1`.
- Imports/outbound dependencies: documented engine getter APIs only.
- Mutable state: none; it translates engine-owned label/pass/address state to
  the runtime-context ABI and never exposes engine table layout.
- Routine responsibility groups: the sole transitional engine access point for
  future tkpkg context consumers, including exact-symbol projection of the
  engine's PC-backed target-reference property.

### `opasm.amigaos.engine`

- Source: `native/motorola68000/amigaos/opasm/opasm_engine.asm`.
- Public surface: session initialization, source/statement collection,
  callback-context construction, pass execution, labels and their PC-backed
  target-reference property, PC/image access, and selector/expression request
  preparation APIs.
- Imports/outbound dependencies: event projection plus default-off progress,
  symbol/expression, and platform-profile observers. The observers own bounded
  private counters, not package semantics or assembly-session state.
- Mutable state: assembly session allocation, source/statement records, label
  table, pass/PC/image state, one byte-presence bit per bounded image address,
  callback context, and diagnostic/event state.
- Routine responsibility groups: session collection and lifecycle; two-pass
  runner; label/image/PC and written-address-presence ownership; callback API;
  request preparation.
- Inbound users: CLI session/source/report components, assembly driver, tkpkg
  service, and test/debug harnesses import the engine API.

### `tkpkg.amigaos.tokenizer_vm`

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_tokenizer_vm.asm`.
- Public entry: `tkpkgTokenizerVmTokenizeLineV1`.
- Imports/outbound dependencies: tkpkg ABI/buffers and TKVM runtime/control/
  state.
- Mutable state: tokenizer request/result and diagnostic/output rendering
  scratch.
- Routine responsibility groups: request/program parsing, TKVM invocation,
  result validation, diagnostic construction, and bounded token rendering.
- Inbound users: tkpkg service.

### `opcore.amigaos.expr_bridge`

- Source: `native/motorola68000/amigaos/opcore/opcore_expr_bridge.asm`.
- Public entries: `opcoreExprEvalOperandV1`, `opcoreExvmEvalOperandV1`,
  `opcoreExvmEvalOperandWithResolverV1`, and `opcoreExvmGetLastResultHighV1`.
- Imports/outbound dependencies: expression VM runtime and the default-off
  symbol/expression observer and the runtime observer imported conditionally by
  `debug/telemetry_macros.i`. The frontend uses reusable VM enter/opcode/leave
  macros; the include owns gates and CCR/register preservation.
- Mutable state: selected opcode version, private ExprVM program length and
  byte buffer, and a private four-byte last-result high word plus two-byte
  availability marker. Parser cursor, paired literal value, and symbol index
  are bounded call-local register state; evaluator state belongs to ExprVM.
- Routine responsibility groups: bounded scalar grammar/literal/symbol-index
  compilation into versioned ExprVM bytecode, optional neutral
  lexical-context resolution before immutable-snapshot fallback, default EXVM
  program selection, and invocation of the ExprVM runtime.
- Inbound users: the tkpkg expression service and operand runtime through the
  evaluation entries; typed consumers read the explicit high-result accessor.

### `exprvm.amigaos.i64_math`

- Source: `native/motorola68000/amigaos/exprvm/exprvm_i64_math.asm`.
- Public entries: `multiplyV1`, `powerV1`, and `divideModuloV1`.
- Imports/outbound dependencies: none; the expression runtime delegates paired
  arithmetic here using high:low register pairs.
- Mutable state: none; `multiplyCore` is a private helper.

### `prvm.amigaos.runtime`

- Source: `native/motorola68000/amigaos/prvm/prvm_runtime.asm`.
- Public entry: `prvmRun68000`.
- Imports/outbound dependencies: PRVM ABI/state/bytecode support and the
  package line-router boundary; plus the default-off runtime observer.
- Mutable state: VM token cursor, checkpoint stack, result records, emitted
  statement fields, and expression resume state.
- Routine responsibility groups: bytecode execution, token access,
  checkpointing, statement-result construction, and expression suspension/
  resume.
- Inbound users: tkpkg service through the line router.

### `tkpkg.amigaos.pipeline`

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm`.
- Public entry: `tkpkgPipelineSetActiveV1`.
- Imports/outbound dependencies: tkpkg ABI/buffers, token policy, state service,
  the compact-table owner binding, and semantic-binding invalidation.
- Mutable state: active package selection and CPU/family/dialect/tokenizer/
  parser locator buffers plus pending/active CPEX property values and presence.
- Routine responsibility groups: request parsing, package hierarchy lookup,
  CPU/family/dialect selection, tokenizer/parser locator resolution, canonical
  CPU word-size/address-bound/byte-order staging, selection commit, and compact
  owner binding only after a successful commit.
- Inbound users: tkpkg service and package-facing setup paths.

### `opasm.amigaos.flow_text_encoding`

- Source: `native/motorola68000/amigaos/opasm/opasm_flow_text_encoding.asm`.
- Public entries: `resetStateV1`, `routeDirectiveV1`, `encodeBytesV1`, custom
  selection, and CDEF/TDEF/EDEF definition entries.
- Imports/outbound dependencies: none; it is session-local flow state.
- Mutable state: active encoding, custom definition table/name/map, definition
  cursor, and escaping scratch.
- Routine responsibility groups: encoding lifecycle, directive routing,
  custom-definition parsing, character mapping, escaping, and byte emission.
- Inbound users: assembly driver.
