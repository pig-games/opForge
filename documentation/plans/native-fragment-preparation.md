# Structural preparation and relocatable module objects

Design for review, updated 2026-10-17. **Structural pipeline not implemented.** Raw
capture disk backing is separately implemented and qualified. Stop for Erik's review before
changing the preparation pipeline. Parity remains paused. The old fragment-envelope
experiment was removed; this proposal does not preserve that selector or its format.
See [current measurements](native-performance-diagnosis.md) and
[capture ownership](native-lazy-preparation.md).

## Intended saving and boundary

Replace “capture all files, then replay all” with a cheap structural scan, followed
by capture, preparation and release of modules in dependency order. The expected
saving is the accumulated captured/prepared source of already published modules.
A near-output, pointer-free `.ofm` object is the proposed ownership boundary: once
it owns everything needed later, source-oriented storage can be released.

An object may be complete or sparse, retaining only referenced whole `.block`
units. The same representation can stay in memory on larger systems or be backed
by disk on smaller systems. This is a hypothesis about memory ownership, not a
promise that another intermediate representation saves time or fits 2 MiB.

## Structural index and configuration ordering

The scan identifies module declarations, imports, exports, whole-unit ranges and
prerequisites, using existing shared lexical/VM facilities. It must avoid full
binding, expression compilation and candidate-plan generation for every record.
Moving that work into an index builder would merely rename the current problem.

Configuration is executable language. Preserve the current ordering: evaluate the
importer's required configuration prefix, including earlier constants/schemas and
`.use ... with` arguments, before configuring a dependency; then prepare dependency
bodies before the importing body. Preserve import source order, root declaration
order, cycle detection and missing/ambiguous-module diagnostics. A module can own
ordered effects as well as exported values.

Unknown conditional dependencies must remain unresolved or escalate to ordinary
preparation; never guess one branch. Mutable definitions invalidate structural
facts that read them. Record the state on which a scan result depends. Explicit
`.cpu`, `.fpu`, `.apollo` and other configuration directives drive package
activation; instruction spelling does not implicitly load capabilities.

## Source order and include suspension

Preserve ordered assignments, macro definitions, shadowing, generated names and
configuration changes. Includes in inactive branches or unexpanded definitions
must not be opened. At an active include, suspend the current source/unit cursor,
expansion state, origin and ownership context; acquire the include and resume in
order. Preserve definition-relative paths and declaration context, while retaining
the invocation's applicable CPU/package selection.

An include may introduce modules or new `.use` dependencies, so discovery can
extend the graph and resume dependency processing. The structural scan is not a
claim that every module edge is known before execution. Generated filenames are
evaluated at their existing shared semantic boundary.

## What publication must own

Objects own bytes, exported typed definitions, schema/member metadata, configuration
values, template signatures/bodies/defaults and the provenance needed for diagnostics
and later instantiation. Use object-local IDs and checked offsets, including nested
region offsets; no persisted process pointers or borrowed capture/parser storage.

Separate three operations: resolving owner-relative storage after movement, remapping
imported object IDs, and relocating emitted section addresses. All fragments support
relocation. Fixed bytes need patches; unresolved expressions, variable-width encoding,
placement-sensitive conditions and loops need existing VM/package recipes. Keep
recipes whose dependencies can change with placement even if currently resolved.
Reverse indexes may map a symbol to both patch sites and dependent recipes.

Retain one loop body and a tree of conditional alternatives, not every iteration
or branch combination. Mutable recipes preserve per-use/invocation/iteration context
and ordered writes or snapshots. Inactive alternatives contribute no live block
roots, file acquisition or package activation. Whole-block inclusion preserves
fallthrough beyond unused internal labels; macros and segments remain templates.
A sparse object records retained units: a later absent-block request triggers source
preparation or a cache miss.

Publication must be transactional: validate and seal imports, member/proxy identities,
typed exports and recipes, then transfer ownership. The current global
`frontend.complete`/materialization boundary needs an explicit bridge to this contract.
Do not free source with outstanding borrowed views. Preserve the no-growth/no-movement
assembly frontier at `beginSizing`; storage ownership must not introduce another
parser, evaluator, CPU encoder or convergence policy.

## Memory bound and disk alternative

The old acquisition floor was 30,216,808 tracked bytes before any replay span ended.
The latest frozen 214-file resident peak is 34,517,176 bytes, including
23,852,392 logical captured bytes. Chunking removes growth waste, not accumulated source ownership.

The proposed bound becomes the maximum simultaneous sum of shared runtime/packages,
structural/configuration index, retained objects/exports, active module/include
capture, scratch/output buffers and publication/allocation overlap. It replaces
all captured modules with an active closure, but resident objects still consume
memory. There is no qualified largest-module, active-closure or object-size bound;
these measurements cannot yield an honest numeric replacement for 30 MB yet.

A disk-backed object store keeps a small directory and bounded I/O buffers resident.
Use a genuinely disk-backed AmigaOS volume: a temp assign resolving to RAM saves
no RAM. The implemented raw-capture spill policy now demonstrates that alternative:
a 1 MiB payload budget reduces the identical full workload's tracked peak from
34,517,176 to 11,839,624 bytes, with no established release-time change. It retains
all source-oriented work and increases allocation/IO churn. Compare future objects
against both this bounded policy and resident capture, not only the old acquisition
floor. Validated identity record reuse subsequently lowers that disk-backed peak
by only 245,120 B / 2.07%, to 11,594,504 B, despite avoiding a 2 MiB replacement
allocation. Another overlap now dominates. The initial 2.40% release slowdown did
not reproduce in controlled follow-up; no whole-build timing change is established.
Matched phase controls save about 76 ms in materialization, with effectively
unchanged postorder work.
See the [isolated comparison](native-performance-diagnosis.md#reuse-an-already-ordered-record-stream).
Neither change establishes an object-size or active-module bound. Extra
scans, validation, rereads and object construction/publication overlap may cost runtime; measure them independently.

## Incremental feasibility and the publication boundary

The raw include-text and prepared declaration-leaf experiments are complete and
[dropped](native-performance-diagnosis.md#cache-experiment-decisions). The latter
proves pointer-free local-ID export/import for tested constants and plain schemas:
five warm entries reuse 698 rows and avoid 1.73% of expression compilations on the
frozen full workload. Peak heap is unchanged and no release speedup is established.
The maintained small-module and macro-heavy controls admit no eligible leaf.
No cache switch or alternate preparation path remains.

The experiment exposes two requirements for the general design:

- Dotted references are globally finalized today. Early module publication must
  preserve unresolved binding recipes or establish a sound closed-module boundary;
  it cannot assume that a currently found name is the eventual preferred binding.
- Preparation-time observations are part of semantics. Importing configuration,
  conditionals and other consumers must see the same values at the same point as
  eager execution. Final folded values alone are insufficient. The discarded
  prototype did not implement its required original-snapshot consistency guard.

Waiting for global completion and then writing an object does not remove the
capture acquisition floor. A useful next implementation must own the semantic
recipes needed after publication and actually release module-oriented capture,
not merely add persistence to the existing all-files pipeline. This remains a
consequential structural change requiring review. No general relocatable code object, sparse linker or early-release pipeline was
qualified by the declaration experiment. The separate raw disk store is storage
qualification, not object publication or avoided preparation.

Before implementation, choose the publication contract and bound the largest
simultaneous active module/include closure, shared state and sealed objects.
Compare disk-backed objects with the now-measured raw-capture spill policy above.
Do not infer a new memory bound from compact leaf objects alone.

A future configured-object fingerprint must cover content, transitive includes
and assets, imported exports/types/templates, compiler and package semantics,
relevant options, search resolution and consumed source-order inputs. Timestamps
are hints, not validity. Persist checked offsets and object-local identities,
never process pointers. Sparse entries must declare missing units and miss or
prepare source when a later request needs one. Unknown dependencies are not hits.

Demand-loaded package chunks remain separate future work. Parity stays paused;
no new observer framework, further macro-only audit or structural implementation
is authorized by this note.
