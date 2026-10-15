# Structural preparation and relocatable module objects

Design for review, 2026-10-15. **Not implemented.** Stop for Erik's review before
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
Current frozen-source chunked peak is 34,324,616 bytes, with about 23.84 MB retained
capture. Chunking removes growth waste, not accumulated source ownership.

The proposed bound becomes the maximum simultaneous sum of shared runtime/packages,
structural/configuration index, retained objects/exports, active module/include
capture, scratch/output buffers and publication/allocation overlap. It replaces
all captured modules with an active closure, but resident objects still consume
memory. There is no qualified largest-module, active-closure or object-size bound;
these measurements cannot yield an honest numeric replacement for 30 MB yet.

A disk-backed object store keeps a small directory and bounded I/O buffers resident.
Use a genuinely disk-backed AmigaOS volume: a temp assign resolving to RAM saves
no RAM. A simpler alternative spills raw capture and rereads it during replay;
it can reduce acquisition residency sooner, but retains source-oriented work.
Compare both against resident objects. Extra scans, validation, rereads and object
construction/publication overlap may cost runtime; measure them independently.

## Incremental feasibility status and next question

The separately authorized in-build raw include-text experiment is complete and
[dropped](native-performance-diagnosis.md#in-build-include-text-reuse): it has hits,
but leaves all per-line preparation intact, adds peak heap and has no measurable
release gain. This is not a configured-object hit-rate measurement. A useful next
slice must reuse prepared meaning while preserving ordered effects and remapping
caller-bound identities. Apply the approximately 1% exact resource gate; do not
add another observer framework.

The next proposed test is one persistent pointer-free leaf-module entry keyed by
content hash and configuration. Its fingerprint must cover transitive include/assets,
imported exports/types/templates, compiler and package semantics, relevant options, search
resolution and source-order inputs actually consumed. Timestamps are hints, not
validity. Test changed dependencies/configuration, cold/warm output equality and
hit rates. An unresolved dependency cannot silently become a valid cache hit.

The persistent leaf slice and structural implementation remain unstarted
at the review boundary. Demand-loaded package chunks are separate future work,
not a prerequisite for this experiment.
