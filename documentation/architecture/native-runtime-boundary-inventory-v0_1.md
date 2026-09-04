# Native Runtime Boundary Inventory v0.1

## Scope and method

The Item 0e platform-observer continuation refreshes the engine, layout, and
operand-runtime manifests for default-off byte-work observers. Layout and
operand runtime gain a dependency only on `debug.amigaos.platform_profile`;
their production ownership and public routine inventory remain unchanged.
Assembly-driver, selection-service, and encode-service hashes also change for
profile-only branch-width corrections required by the larger debug build.
Release-mode instructions remain the reference; no package semantics move.

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

Item 13 refreshed the audited hashes for `opasm.amigaos.assembly_driver`,
`opasm.amigaos.engine`, and `opcore.amigaos.expr_bridge` after
semantics-preserving explicit branch-width corrections required by stricter
resolved-branch assembly. Their routines, imports, sections, diagnostic paths,
ownership decisions, and dependency directions are unchanged.

Item 38 refreshes seven audited hashes after measured full-product capacity,
packed-storage, label-indexing, and layout-stability work. Imports and section
ownership remain unchanged. The complete inventory adds one layout helper, five
private engine label-index helpers, and one expression-bridge diagnostic path;
all remain architecture-neutral and inside their existing owners.

Item 14 refreshed the complete audited manifest after adding the generic
compact-table selector/encoder path and its package-owned fixed-program
execution. The refresh includes the affected opasm bridge/layout routines and
the tkpkg service, pipeline, selection, operand, encoding, and compact-table
owners; no CPU-family semantic owner moved into the generic native runtime.

Item 15 refreshed the complete audited manifest after directly porting Rust's
CPU-neutral CSEM-v2 scalar/register projection and encoding behavior. The
refresh covers the assembly-driver, operand-evaluation, selection, operand,
encoding, engine-shape, and expression-bridge owners. The exact Item 13.1
package remains unchanged; register names, encodings, ranges, field layouts,
endianness, and emitted opcode values remain package data.

Item 16 refreshed the complete audited manifest after directly porting Rust's
CPU-neutral CPRD scope lookup and frozen OPRD-v1 base-record execution. The
new operand-record service interprets only opaque ids, owner scopes, neutral
register references, scalar inputs, and schema opcodes. It validates UTF-8 and
the entire bounded CPRD owner/program set before selection, and rejects any request interval
that overlaps its distinct result buffer before reading or materializing bytes.
The exact Item 13.1 package and every family definition remain unchanged.

Item 17 refreshed the complete audited manifest after directly porting the
remaining neutral selector projections, semantic sequences, InputFields-v6
encoding, absolute fixups, and package-owned semantic rejection needed by the
base 68000 movement/control fixtures. The selection, operand, encoding, and
engine modules still interpret only package-owned plans and neutral values;
no CPU, mnemonic, register, addressing-mode, or opcode authority moved into
the generic native runtime. The exact Rust-built package remains unchanged.

Item 18 refreshed the complete audited manifest after directly porting Rust's
neutral VALUE_VM v1/v2 operations, semantic-input projection, package-shape
classification, and CSEM-v5 branch-program execution needed by the base 68000
ALU/bit/shift fixtures and one explicit-word branch regression exposed by the
single native completion run. The native modules interpret only opaque package
records, neutral scalar/register/direct inputs, and package-owned diagnostics;
automatic branch sizing and multi-pass stability remain Item 19. The exact
Rust-built package and every family definition remain unchanged.

Item 19 refreshed the complete audited manifest after directly porting Rust's
bounded whole-layout convergence, exact PC-backed label refresh, package-owned
branch candidate selection, and explicit range-diagnostic behavior. The native
frontend still has no Rust family handler; when no frontend shape exists, the
generic selector now lets an exact package row supply its opaque shape and
neutral operand plan. No CPU, mnemonic, register, addressing-mode, opcode,
branch width, or legality authority moved into the generic runtime. The exact
Rust-built package and every family definition remain unchanged.

Item 24 refreshed the assembly-driver and selection-service manifests after
the single complete native run exposed two final neutral-runtime gaps. The
driver now consults original statement syntax only to distinguish an explicit
`.pack` directive from the package-owned bare `PACK` mnemonic after normalized
text metadata removed the sigil. The selection service now mirrors Rust's
generic `out_of_rangeN.minX.maxY` rejection projection and scalar capture.
Directive meaning, operand bounds, diagnostics, CPU availability, and emitted
instruction bytes remain owned by the unchanged Rust-built package and VM
definitions.

Item 25 adds `tkpkg.amigaos.state_service` and refreshes its four neutral
callers. The service selects and interprets the frozen STVM program, restores
package profile defaults, applies package-declared directive transitions, and
answers opaque state requirements. CPU names, directive names, target values,
profile legality, and selector diagnostics remain in the unchanged Rust-built
package; the native runtime contains no FPU or Apollo target table.

Performance-plan Item 0d refreshes the audited snapshots for the coarse native
runtime observation sites. The affected VM and service owners conditionally
import `debug.amigaos.runtime_profile`; that dependency is a default-off,
CPU-neutral passive observer and owns no package, CPU, instruction, selection,
encoding, expression, diagnostic, or output semantics. Disabled compositions
retain the previously recorded Hunk bytes exactly.

## Dependency direction

```text
CLI/session frontend -> assembly driver -> engine callback API
                                    -> flow owners / text-encoding owner
                                    -> tkpkg bridge -> tkpkg service facade
tkpkg service facade -> pipeline, tokenizer VM, PRVM line router,
                        expression bridge, package loader, operand-record service
pipeline -> package hierarchy / CPU-family-dialect locators
         -> state service -> package-owned STVM profiles/keys/transitions
expression bridge -> expression VM runtime
```

The desired direction is toward narrow service or runtime contracts. Item
5.7.2 removes the obsolete `tkpkg.amigaos.service -> opasm.amigaos.engine`
import; the engine-context adapter is now the sole tkpkg engine reader.

## Audited modules

### `tkpkg.amigaos.state_service` (Item 25 package-state interpreter)

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_state_service.asm`.
- Public entries: `initializeActiveV1`, `resetActiveV1`, `applyDirectiveV1`,
  `getFlagV1`, and `requirementAllowsV1`.
- Imports/outbound dependencies: `tkpkg.amigaos.buffers`; the default-off
  `debug.amigaos.runtime_profile` observer in Item 0d builds.
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
- Decision: this is a CPU-neutral serialized-program interpreter. It must not
  contain family, CPU, FPU, Apollo, mnemonic, register, or target-value tables.

### `opasm.amigaos.assembly_driver` (NR-006, mandatory decomposition)

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
  dispatch, including original-source sigil disambiguation when a normalized
  directive name collides with a package mnemonic; structural-flow state
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
- Decision: orchestration stays here. Item 5.8 moves non-structural directive
  text classification to `opasm.amigaos.directive_router`; the driver consumes
  only its numeric result. Item 5.8.1 moves future-statement structural scans
  to `opasm.amigaos.flow_navigation`; the driver retains only the explicit
  `.case` operand-evaluation callback pending Item 5.9. This is an
  ownership-only extraction, followed by operand
  evaluation (5.9), selector adaptation (5.9.1), text (5.9.3), and layout
  (5.9.4). Numeric directive sizing, little-endian byte packing, and image
  append now belong to `opasm.amigaos.directive_data`; the driver supplies only
  statement-aware count and evaluation callbacks. Repeated directive/scanner comparisons are candidates
  for one bounded shared utility only after the router extraction proves need.
  Callback routes that skip structural statements must explicitly return their
  next statement index; no callback may rely on router scratch-register state.

### `opasm.amigaos.directive_router` (Item 5.8 ownership split)

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
- Decision: this is a routing-only split. It neither owns structural-flow
  terminator scans nor enables CPU, family, dialect, instruction, segment, or
  statement semantics.

### `opasm.amigaos.operand_eval` (Item 5.9 ownership split)

- Source: `native/motorola68000/amigaos/opasm/opasm_operand_eval.asm`.
- Public entries: selected-instruction request construction, textual expression
  request construction, their evaluation-extension adapters, and bounded
  materialization of imported aliases supplied through the callback ABI.
- Imports/outbound dependencies: callback ABI, engine request builders, and the
  flow-scope owner's bounded active-label alias query.
- Mutable state: a bounded evaluation-only snapshot of local, imported, and
  qualified/global label names and values;
  the driver still supplies its service frame and owns request-length state,
  dispatch, diagnostics, and fallback policy.
- Decision: this owner constructs engine request envelopes and projects
  active-scope and imported aliases ahead of the unchanged qualified/global
  label snapshot. It also supplies the same lexical-scope-first resolver to the
  expression bridge when a directive expression misses that immutable
  snapshot. Imported names are resolved only through the CLI-owned callback;
  this module does not own module visibility. It does not select, encode, emit,
  own expression syntax, or own layout behavior.

### `opasm.amigaos.directive_data` (Item 5.9.2 ownership split)

- Source: `native/motorola68000/amigaos/opasm/opasm_directive_data.asm`.
- Public entries: `sizeNumericDirectiveV1` and `emitNumericDirectiveV1`.
- Imports/outbound dependencies: engine image append; the driver supplies its
  existing comma-count and statement-aware operand-resolution callbacks.
- Mutable state: per-session callback pointers, unit-width scratch, and a
  four-byte packing buffer.
- Routine responsibility groups: numeric list sizing, byte range validation,
  MOS little-endian packing, and image append.
- Decision: this is an ownership-only split. Existing directive classification,
  two-pass orchestration, expression evaluation, diagnostic status, and CPU
  semantics stay at their existing boundaries. In debug-contract builds it
  emits structured `EVENT_DIRECTIVE_DATA` records after resolution and append;
  release builds do not execute that instrumentation.

### `opasm.amigaos.directive_text` (Item 5.9.3 ownership split)

- Source: `native/motorola68000/amigaos/opasm/opasm_directive_text.asm`.
- Public entries: `sizeTextDirectiveV1` and `emitTextDirectiveV1`.
- Decision: owns text-mode size, prefix/suffix, and image emission while the
  driver supplies existing parsed scratch and encoding callbacks.

### `opasm.amigaos.layout` (Item 5.9.4 ownership split)

- Source: `native/motorola68000/amigaos/opasm/opasm_layout.asm`.
- Public entries: region/section/place state transitions, bounded layout-name
  request APIs, `alignCursorV1`, and `alignPadV1`.
- Imports/outbound dependencies: none.
- Mutable state: region/section/place counters, names, bounds, cursors,
  alignment values, placement indices, Hunk memory attributes, and scratch
  storage.
- Routine responsibility groups: overflow-safe positive alignment,
  power-of-two padding arithmetic, bounded layout-name copy/comparison,
  region/section/place validation and transitions, including sequential `.pack`
  placement through the same transition owner, and word/long table-index
  calculation. The driver retains statement tokenization, callback dispatch,
  and engine/image projection only.
- Decision: layout state and all region/section/place transitions are owned by
  this module. The completed transfer preserves existing arithmetic and adds no
  layout syntax or semantics; the assembly driver has no direct layout-state
  access.

### `tkpkg.amigaos.service` (NR-002/003/004, mandatory decomposition)

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm`.
- Public entries: `bootstrapV1` and `dispatchV1`.
- Imports/outbound dependencies: tkpkg ABI/buffers, dedicated request-lifecycle,
  status-projection, parser-adapter, expression-service, and selection-service
  owners; operand-record service, engine, expression bridge, package loader,
  pipeline, and tokenizer VM; plus the default-off Item 0d runtime observer.
  The expression service now reaches pass/finalization state only through the
  neutral `tkpkg.amigaos.runtime_context` façade.
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
  `tkpkg.amigaos.expression_service`; Item 5.7.1 then migrates its pass and
  finalized-label access to `tkpkg.amigaos.runtime_context` and deletes the
  temporary adapter. The facade retains package-contract validation only until
  the neutral-context migration. Item 5.6 moves selection
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
- Public entries: `selectInstructionV1`, `buildSelectedEnvelopeV1`,
  `noOutputErrorV1`, and `tkpkgProjectBoundedRegisterV1`.
- Imports/outbound dependencies: tkpkg ABI/buffers, operand runtime, neutral
  runtime context, the expression bridge transition boundary, and the
  default-off Item 0d runtime observer.
- Mutable state: selected request envelope and candidate traversal cursor; the
  unchanged operand scratch state is shared through the internal selection-state
  module. Item 21 adds one bounded deferred-rejection text buffer so traversal
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
- Decision: this module delegates existing plan interpretation to
  `tkpkg.amigaos.operand_runtime` and reads the session pass through the neutral
  runtime context. Item 15 adds only neutral package record decoding and input
  projection; Item 20 completes the package-declared register-list,
  identity-scale, and diagnostic-capture projections. All target names,
  register indices, accepted values, field meanings, diagnostic templates, and
  instruction semantics remain package-owned. Item 21 adds only the neutral
  named-register predicate and Rust-equivalent owner-priority deferral required
  to consume the frozen m68010 rows. Item 22 adds the neutral `xp1:`
  expression-path subset used by the frozen full-extension rows plus Rust's
  nonidentity-scale predicate. Its neutral indirect-container walker preserves
  Rust's equivalent `(item0,item1,...)` and `item0(item1,...)` tuple spellings;
  operand indices, tuple paths, qualifiers, classes, fields, and rejection
  meaning remain package data. Item 23 adds Rust's neutral direct/indirect
  call-argument register projection and distinct-register predicate for the
  frozen first 68020 later-family group. The native visitor accepts the same
  slash-list and colon-call argument separators, while all CPU, instruction,
  register, opcode, and legality authority remains in the exact Rust-built
  package. Item 24 extends that same neutral boundary with Rust's postfix-brace
  call splitting, selected scalar/member/indirect-tuple-register projection,
  and the direct-identifier relocation-target subset required by the frozen
  remaining 68020 bit-field rows. Target expression values are still evaluated
  only by the existing fixup bridge, and member fields, tuple items, register
  classes, encodings, opcodes, legality, and diagnostics remain package-owned.
  The final wrapper-discovered range case adds Rust's neutral signed
  `out_of_range` predicate and `{value}` capture without naming an instruction
  or owning its package-declared bounds. Item 34 adds Rust's neutral
  `bounded_regN.classC.minM.maxX[.outside]` projection and transports the
  target-reference bit for an exact resolved label. Item 35 additionally
  transports Rust's independent per-input unresolved bit so package-owned
  placeholder policy runs before forward-label projection. Register classes,
  bounds, inversion, tuple paths, and fixup programs remain package-owned;
  whether a resolved symbol is PC-backed remains engine-owned.

### `tkpkg.amigaos.operand_runtime` (NR-004, Item 5.6.1 ownership split)

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_operand_runtime.asm`.
- Public entry: `tkpkgMselTryBuildCandidateV1`.
- Imports/outbound dependencies: tkpkg buffers, private selection state, neutral
  runtime context, and the expression bridge transition boundary.
- Mutable state: reads and writes the preserved selection-state scratch layout;
  it does not own package selection or selected-output diagnostics.
- Routine responsibility groups: plan-tag dispatch, operand-span normalization,
  top-level neutral operand-list splitting, expression evaluation, and
  candidate-envelope construction.
- Decision: this is a file-boundary extraction only. Its legacy expression
  bridge receives context-owned copies of symbol names, values, and stability;
  it no longer receives engine label-table storage. Item 15 locates and
  evaluates opaque semantic operand spans without interpreting register,
  addressing-mode, mnemonic, or CPU spellings; those decisions remain in the
  package projection rows.
  Item 35 clears expression instability at each direct semantic evaluation so
  one input or statement cannot leak unresolved state into the next fixup
  record; the selection service transports that neutral result.

### `tkpkg.amigaos.encode_service` (NR-004, Item 5.6.2 ownership split)

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_encode_service.asm`.
- Public entries: `encodeInstructionV1` and `encodeSelectedInstructionV1`.
- Imports/outbound dependencies: tkpkg ABI/buffers, private selection state,
  the existing selection-service boundary, and the generic compact-table
  boundary; plus the default-off Item 0d runtime observer.
- Mutable state: writes the same existing package-service output buffer; it does
  not own pipeline selection, package loading, or status projection.
- Routine responsibility groups: selected-envelope encoding, legacy
  package-table lookup, neutral CSEM owner/program lookup, direct CSEM-v2
  Literal/Scalar/Fields execution with bounds/overlap/endianness validation,
  compact fixed-row delegation, and encoded-output construction.
- Decision: Item 14 retains this module as the sole neutral bytecode executor
  and routes compact fixed-row discovery through a separate bounded package
  reader. The executor accepts only its existing literal/operand/END contract,
  now rejects trailing bytes and output overflow, and contains no CPU, family,
  dialect, mnemonic, register, addressing-mode, or target-opcode authority.
  Item 15 extends that same neutral executor with the Rust CSEM-v2 wire
  operations; it does not add any native target dispatch.

### `tkpkg.amigaos.compact_table` (NR-004, Item 14 compact package activation)

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_compact_table.asm`.
- Public entry: `findFixedProgramFromRequestV1`.
- Imports/outbound dependencies: tkpkg ABI/buffers and the existing neutral
  scoped-owner/string/bounds helpers in the selection service.
- Mutable state: bounded compact-reader scratch fields in tkpkg buffers and the
  existing output scratch buffer while reconstructing prefix-compressed strings.
- Routine responsibility groups: exact CTBL version validation, bounded owner
  and string-table reconstruction, dialect/CPU/family scope-order selection
  among matching rows, ambiguity rejection, and program-byte location.
- Decision: this module interprets only the frozen CPU-neutral compact wire
  format. All names, target meanings, and emitted opcode bytes remain package
  data; later semantic program versions, operand records, fixups, and branch
  convergence remain outside the Item 14 boundary.

### `tkpkg.amigaos.operand_record_service` (NR-004, Items 16 and 22 operand records)

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_operand_record_service.asm`.
- Public entry: `executeRequestV1`; it parses one bounded neutral request,
  resolves an opaque CPRD id by dialect/CPU/family precedence, and executes one
  exact OPRD schema-v1 base program or schema-v2 nested-address program.
- Imports/outbound dependencies: the tkpkg ABI and shared buffers; plus the
  default-off Item 0d runtime observer.
- Mutable state: bounded request cursors, active-owner indices, selected-program
  metadata, and the dedicated neutral result buffer (24-byte v1 or 40-byte v2).
- Routine responsibility groups: little-endian bounded CPRD reading; complete
  UTF-8, duplicate-owner/duplicate-id, and v1-v3 program-set validation before scoped
  owner/id matching; exact program-shape and END validation; request/result
  non-overlap enforcement; neutral register,
  indirect/update, displacement/base, indexed/width/scale, absolute, and
  immediate result materialization; Item 22 nested base, optional base
  displacement, optional index, indirection, and optional outer displacement
  materialization; missing-input and malformed-data rejection.
- Decision: this module is a direct native port of the Rust package and operand-
  record VM boundary. It owns no CPU, family, dialect, mnemonic, register name,
  addressing-mode spelling, or encoding byte. Schema-v2 pair/list/field records,
  schema v3, instruction encoding, fixups, and branch convergence remain outside
  the completed Item 22 boundary.

### `tkpkg.amigaos.runtime_context` (NR-005, Item 5.7 ownership split)

- Source: `native/motorola68000/amigaos/tkpkg/tkpkg_runtime_context.asm`.
- Public entries: `getAbiVersionV1`, `getPassV1`, `getAddressV1`,
  `lookupSymbolV1`, `isSymbolTargetReferenceV1`,
  `getSymbolStabilityTableV1`, `getSymbolTableSnapshotV1`,
  `reportDiagnosticV1`, and `getLastDiagnosticV1`.
- Imports/outbound dependencies: only the engine-context adapter.
- Mutable state: private neutral diagnostic, symbol-stability, and bounded
  copied symbol-table records; none is engine, CLI, or package-service storage.
- Routine responsibility groups: versioned read-only context projection,
  bounded diagnostic handoff, bounded stability snapshot materialization, and
  read-only projection of engine-owned label target-reference metadata.
- Decision: Items 5.7.1 and 5.7.2 migrate expression, selection, and operand
  consumers through bounded context snapshots. Neither change adds CPU, family,
  dialect, instruction, selector, plan-tag, or encoding support.

### `tkpkg.amigaos.engine_context_adapter` (NR-005, Item 5.7 ownership split)

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
- Decision: this adapter remains the sole transitional engine-state reader.
  Items 5.7.1 and 5.7.2 have migrated expression, selection, and operand
  consumers. Parent native parity Item 7.7 is the latest permitted removal
  milestone: module/import integration must provide the neutral context without
  a tkpkg-to-opasm import, then delete this adapter and its inventory/no-growth
  allowance. This work adds no CPU or package semantics.

### `opasm.amigaos.engine` (NR-001, conditional decomposition)

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
- Decision: retain cohesive after the Item 5.11 conditional audit. It imports
  event projection plus the default-off observation dependencies and owns one
  assembly-session aggregate: collected
  statements, pass/PC/image/label state, callback context, and bounded request
  serialization over that state. The request writers do not select packages or
  encode output, and the selected-shape helper has no mnemonic classifier or
  package dependency. Moving these routines would split access to the same
  state without removing a prohibited edge. Later CPU/selector semantic
  remediation remains separately governed and is not authorized by this audit.
  Item 34 exposes the already-owned PC-backed bit for the last exactly resolved
  label; it does not infer package semantics or target names. Item 15 only
  corrects the generic selected-shape hint so a top-level comma is
  not misclassified as one legacy immediate operand; the package projection
  remains authoritative for the composite operand list. Item 37 records and
  clears bounded written-address presence alongside the already-owned image so
  artifact writers can distinguish a Rust ImageStore gap from an explicitly
  written zero; it does not interpret output formats or target semantics.
  Item 38 widens only the label-specific statement and symbol rows to the
  measured 108-byte native representation required by Rust's 107-byte fully
  scoped product maximum; generic token rows and all symbol semantics remain
  unchanged.

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

### `opcore.amigaos.expr_bridge` (NR-008, retained cohesive frontend)

- Source: `native/motorola68000/amigaos/opcore/opcore_expr_bridge.asm`.
- Public entries: `opcoreExprEvalOperandV1`, `opcoreExvmEvalOperandV1`, and
  `opcoreExvmEvalOperandWithResolverV1`.
- Imports/outbound dependencies: expression VM runtime and the default-off
  Item 0c symbol/expression and Item 0d runtime observers.
- Mutable state: selected opcode version plus the private ExprVM program length
  and byte buffer. Parser cursor, literal value, and symbol index are bounded
  call-local register state; evaluator state belongs to the ExprVM runtime.
- Routine responsibility groups: bounded scalar grammar/literal/symbol-index
  compilation into versioned ExprVM bytecode, optional neutral
  lexical-context resolution before immutable-snapshot fallback, default EXVM
  program selection, and invocation of the ExprVM runtime.
- Inbound users: the tkpkg expression service and operand runtime through the
  two documented public entries.
- Decision: retain cohesive. This module is the sole native scalar
  text-to-ExprVM frontend; its parser and emitter share one cursor/register ABI
  and private program buffer. It owns no request-envelope, diagnostic, evaluator,
  package-selection, or engine-context policy and imports only the ExprVM
  runtime. The optional resolver is call-scoped policy supplied by the owner;
  it does not expose or import engine state. All compiler helpers are private
  after Item 5.10. The long-term owner remains this bridge until a package
  parser supplies ExprVM bytecode directly; that future replacement, not a
  line-count split, is its deletion criterion. Item 38 keeps its read-only
  label-row stride aligned with the engine's measured 108-byte native row; it
  adds no grammar, lookup, or evaluation behavior.

### `prvm.amigaos.runtime` (NR-005, retain cohesive)

- Source: `native/motorola68000/amigaos/prvm/prvm_runtime.asm`.
- Public entry: `prvmRun68000`.
- Imports/outbound dependencies: PRVM ABI/state/bytecode support and the
  package line-router boundary; plus the default-off Item 0d runtime observer.
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
- Decision: retain cohesive after the Item 5.11 conditional audit. The sole
  public transaction parses one request, resolves the package-owned CPU,
  family, dialect, token-policy, tokenizer, and parser hierarchy, and commits
  only a complete selection. It imports no engine or CLI state. Its locator and
  string helpers are private and share the traversal cursor/register contract
  used by every resolution stage. Similar low-level readers elsewhere remain a
  duplication finding, but consolidating them now would add a cross-runtime
  utility dependency without an independent state or semantic boundary.

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
