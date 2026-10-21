# Stored module assembly interfaces

Status: design proposal, 2026-10-21. Implementation is not authorized by this
design task. Preserve `--reference-modules` and the frozen working path.

## Outcome and current gap

Assemble each configured module against dependency interfaces held in `.ofm`,
write its own object, then release its compiler storage. Keep only small module
handles and bounded interface views between consumers. Final-link details remain
in objects; consumers do not load dependency code or relocation tables merely
to compile an instruction, expression or template call.

Currently all 185 self-host dependency objects have empty semantic sections.
Publication installs values/types and template capsules into the shared frontend
before stripping semantics from the object. The next private compiler snapshots
and installs those dependencies again. Persisting today's semantic blob while
keeping this cycle would add disk storage without replacing the resident owners.

The proposed boundary includes both persistence and direct consumption. It is
not a persistent incremental-build cache contract. Source/configuration/package
fingerprinting and cross-build invalidation remain separate work.

## Stored representation

One immutable object contains these indexed regions, addressed by checked
base-relative or region-relative offsets:

| Region | Contents and consumer |
| --- | --- |
| Identity and dependencies | Configured module identity, package/context identities and ordered dependency identities; used by assembly and linking |
| Assembly interface | Public name/facet index, typed values, nominal schemas, member keys, address identities, binary macros/segments/statements, signatures/defaults, declaration contexts and private dependency closure |
| Fragments | Code/data, BSS extents, alignment and whole-block ownership; used by linking |
| Fixups and deferred work | Target identities, addends, expressions and supported layout recipes; used by linking |
| Provenance | Origins and definition-relative file context for diagnostics, listings and active expansion effects |

The public index maps a spelling to its available facets, not one untyped value.
A value and template may share a spelling. Hashes accelerate binding; exact name
comparison resolves collisions. Subsequent processing uses numeric identities.
Private closure entities are addressable by identity but absent from public lookup.

An entity identity is `(dependency ordinal, entity ID)`, two 32-bit fields.
Dependency zero denotes the containing object. Other ordinals select its stored
dependency directory; IDs are local to the selected configured module. Entity
rows carry kind/facet information. An expansion referring through a provider's
dependency directory uses that provider as its resolution context, not the
consumer's directory. No process pointers or build-global symbol IDs are stored.

Configuration identity includes the actual typed configuration and applicable
package/context identities; hash equality alone is not identity. Address-bearing
values retain symbolic fragment identities. Nominal type/member identities must
not collapse because two records have equal contents. These rules apply inside
compound values and template closures as well as top-level exports.

Keep one current representation and shared VM contracts. Integrity tags and
bounds validation do not introduce native contract-version dispatch.

## Minimal resident definitions

These are proposed 32-bit logical layouts, not a frozen public ABI. Array
position supplies a record's ID; no per-row self ID or reserved padding is needed.

| Record | Fields | Bytes |
| --- | --- | ---: |
| `ModuleHandle` | Object ID, configuration reference, interface offset, export-index offset, export count, state | 24 |
| `Dependency` | Module handle, importing scope, selection/mapping options reference | 12 |
| `ImportedBinding` | Consumer-local binding ID, dependency ordinal, provider entity ID | 12 |
| `EntityRef` | Dependency ordinal, entity ID | 8 |

The object-store catalogue owns filenames, names, file bounds and immutable
configuration payloads through bounded backing access. Their capacity is counted;
they are not hidden resident allocations behind each small handle. Keep a bounded
pool of open files rather than an OS file handle for every object.

The active module owns its dependency rows in source order, including repeated
aliases and their selection/mapping policies. Alias spellings are ordinary local
bindings. Allocate an imported binding only when resolution needs it. The module
handle is shared; a consumer does not import the provider's complete symbol table.

```text
RAM                                      Backing storage
module handles ------------------------> configured module catalogue
active module
  local scopes/bindings
  used import references --------------> dependency .ofm assembly interfaces
  binary input / expansion window        values, types, templates, closure
  current preparation/layout work
  bounded reader/writer buffers --------> current output .ofm

completed module: release local owners; retain handle
final link: read stored placement/fixup data and selected fragments
```

One active compiler still needs local mutable symbols, scopes, current values,
package context, include/expansion cursors and unresolved local work. Those are
not dependency-interface bookkeeping and cannot simply disappear. Local tables
and large values must use checked backing views when they exceed working windows.
Plans/packed records also need explicit ownership, but their existing measured
capacities are not counted as savings in this proposal.

## Semantic and lifetime rules

- Imported interfaces are immutable snapshots. Consumer mutation/expansion state
  is local; never mutate a cached dependency value or schema.
- Preserve provider declaration binding, caller argument binding, expansion
  hygiene, overload/signature selection and private template closure separately.
  Do not resolve provider-private references against caller spellings.
- Includes/file effects open only when active or expanded. Preserve definition
  origin and include suspension. Required semantic strings and generated spellings
  remain binary-owned data; source text is not a later parsing fallback.
- `.use` selection controls visibility, not block liveness. Whole referenced
  blocks and required module effects survive; internal fall-through is retained.
- Values needed for compile-time decisions must be available before consuming
  them. Deferrable addresses/layout expressions become supported recipes, never
  guessed absolute values. This design does not add new parity or relaxation rules.
- Schedule missing dependencies without retaining nested complete frontends.
  Suspend parent state into backing storage. Address-only references can remain
  symbolic; compile-time cycles need existing semantics or an explicit diagnostic.
- Writer and readers exchange IDs, checked owned copies or scoped leases. Cache
  eviction cannot invalidate a live borrow; leases do not escape into saved plans.
- Stream interface regions and completed fragments to unpublished object storage.
  Avoid a second full interface image or recursive closure-copy graph in RAM.
  Validate before publishing the handle. Failed publication cleans only its own
  unpublished storage, and cannot replay against partially installed semantics.
- After publication, retire the producer's lexical/value/template owners. Keep
  only its handle, required small scheduling/link state and bounded cached views.

## Recommended implementation step

**Make stored interfaces the canonical dependency owner throughout the normal CLI.**
This is a compiler ownership change spanning publication, binding, values and
expansion, rather than another serializer-only or macro-only experiment.

1. Extend the current object/interface capture and validation boundary to own
   complete supported assembly interfaces, including template/statement closure.
   Reuse existing typed-value and capsule machinery where its identities are
   suitable; replace dependence on live frontend bases and global IDs.
2. Add a bounded interface reader/export resolver over object storage. Centralize
   entity, member and typed-value access; use a separate template reader for
   signatures/defaults/body cursors. Both share object views, not duplicate caches.
3. Route private module initialization and the root frontend through these
   readers. Bind only used entities, retaining module-qualified identities through
   expression evaluation and template expansion. Store template definitions in
   the provider object; generated caller-local bindings belong to the consumer.
4. Replace publication into the shared frontend and dependency snapshot/install
   copies. Keep only minimal numeric address/reachability bridges required by the
   existing final driver, populated for actual uses. Do not rebuild an equivalent
   global semantic database behind a compatibility adapter.
5. Bound local lexical/import working storage before dependency resolution starts,
   using existing backing primitives. The budget includes local data as well as
   borrowed interface access; private public/global facets cannot pin the old
   complete lexical payload. Keep backing live through interface export/cleanup.

The root must consume interfaces directly, but need not itself become an `.ofm`
in this step. Preserve current fragment layout, block selection, relocation and
Hunk emission. Closing the root's object-admission boundary and removing the
carrier stream are separate work; do not disguise those as completed here.

Separate implementation checkpoints may establish writing, reading and normal
consumer integration. Until integration removes the copied owners, they are
incomplete checkpoints, not delivered memory savings. Use the existing request
policy and `--reference-modules`; add no stack of experiment switches. Never
silently mix partial new publication with reference execution on failure.

## Expected memory saving and runtime risk

The unchanged frozen self-host peaks at 6,713,680 B while privately preparing
`binary_app`, before frontend retirement. The following capacities are measured:

| Existing private owners to replace/bound together | Capacity |
| --- | ---: |
| Lexical entry and name payloads | 1,048,576 B |
| Imported values/state | 231,680 B |
| **Gross affected capacity** | **1,280,256 B** |

These are mixed local/import owners, not 1.22 MiB of proved redundant imports.
Retained local needs must fit in the replacement budget, not be subtracted twice.
Set **512 KiB** as the provisional combined capacity ceiling for their replacement
local working storage, imported bindings, interface/catalogue I/O windows, added
directory ownership and old/new allocation overlap. Avoid allocating that ceiling
in addition to complete old owners. A provisional split is 256 KiB local lexical
and value windows, 64 KiB import/reference working tables, 64 KiB read cache,
64 KiB writer buffer, and 64 KiB directories/leases/growth overlap. Larger owners
must spill; these are design budgets requiring implementation proof.

That gives a capacity-based forecast of **755,968 B net (738.25 KiB, 11.3%)** at
the identified peak: 1,280,256 minus 524,288 B. It is not a measured whole-build
reduction. Additional shared-interface retirement may save more, but the shared
4,364,712 B is not individually attributed and receives **zero credit** here.
Neither the 262,144 B plan arena nor existing bridge/template/visibility capacities
are credited. Added code/static costs and any new peak must be measured separately.

If other phases do not become larger, the forecast heap is **5,957,712 B**.
Keeping current linked reservations and stack gives a process lower bound of
**6,881,644 B (6.56 MiB)**, still 65,900 B above the 6.5 MiB budget before other
costs and executable growth. No constrained-memory fit claim follows.

This step replaces the separately proposed 632 KiB lexical-window repair as the
recommended next implementation: it includes bounded local storage while removing
the dependency-copy cycle. The 512 KiB ceiling is a hypothesis; inspect existing
owner counters during integration and revise or stop if the required working set
cannot meet it. Do not manufacture a savings result by excluding local state,
catalogue data, pinned leases or serialization overlap.

Avoiding repeated dependency snapshots/copies should reduce CPU work; on-demand
reads can offset that benefit. No numerical runtime gain is predicted. Index and
cache access must be batched so this does not reproduce fragmented prepared-record
I/O. Existing counters/telemetry suffice; add no observer framework.

## Completion and comparison

- Exact live Rust comparison for binaries and Hunk, plus a fresh full-source native
  self-host. Cover values/types/compound addresses, aliases/selection/mapping,
  shadowing, same-name facets, templates/defaults/private closure/caller context,
  active include origins and source-order effects. Fix any Rust oracle issue first.
- All 185 existing dependency objects retain the interfaces their consumers need;
  every normal consumer, including the root, uses the stored interfaces. No hidden
  snapshot/install or bulk template copy remains in the candidate dependency path.
- Existing ownership telemetry must show reduced total peak, bounded replacement
  capacities at earlier phases as well as the previous maximum, and zero terminal
  ownership. Report catalogue/configuration and linked-image growth explicitly.
  The forecast is about 0.7 MiB net; below the phase's approximately 1% resource
  threshold is rejection, not successful architectural cleanup.
- Keep frozen old/new cases, identical input/configuration/package identities and
  fresh Rust oracles. Include a small multi-module and a macro-heavy workload.
  Separate release timing from memory instrumentation. Full comparative timings
  remain paused pending the existing root-handling decision or explicit agreement;
  correctness/memory proof is not a speed claim. Use existing native proof rules.

Current measurements and engineering limits remain in
[native runtime and memory diagnosis](native-performance-diagnosis.md). Current
implemented behavior remains in
[per-module object contracts](native-fragment-preparation.md); this proposal does
not change those implementation claims.
