# Module preparation and relocatable objects

Follow the [current 8 MiB budget and performance sequence](native-performance-diagnosis.md#current-assignment-and-decision-points).
Erik has now authorized work on the first-reference module approach below.
Its first saved-image component is implemented; CLI scheduling and early native
module lowering remain unimplemented. Persistent cross-build reuse is not enabled.

Status, 2026-10-18: dependency-first preparation now interleaves with module
configuration/discovery with resident capture. Completed module captures retire after their numeric
records and templates have been published into the shared semantic owner.
Disk-backed prepared module ranges now survive global binding/layout and reachability
through bounded views, allowing frontend scratch to retire before assembly input is
materialized. Final binding, layout and emission still run globally. This is an
in-build ownership boundary, not a persistent `.ofm` object implementation. Sequential lexical
sealing now has qualified-source binding repaired and reuses its bounded cache
buffers. Current release self-hosting succeeds exactly; the extra instrumented
current-source run times out during global completion. Frozen resource comparisons
remain valid, but that timeout supplies no final peak or completion. Parity remains
paused.
See [measurements](native-performance-diagnosis.md) and
[capture ownership](native-lazy-preparation.md).

## Implemented publication boundary

The discovery graph can yield a dependency-complete module before a later import
requires another file. A successful shared-frontend replay publishes owned numeric
records, definitions and templates, then acknowledges that graph node and retires
its raw capture interval. A failed replay is never acknowledged. Dependency and
source order, whole-block fallthrough and normal global reachability remain intact.

Configuration and semantic sessions own independent scratch and graph storage.
Only one session is active at the stable frontend address; switching restores
shared VM control. Append-only immutable configuration snapshots are transferred
by spelling into semantic identities. Previously consumed section maps retain
their state. Repeated `.use` configurations must name the same complete set of
keys and equal values, independent of key order, matching Rust.

Before requesting another publication, the scheduler tests whether configuration
has closed. Once it has, it transfers the remaining schedule, seeds final snapshots,
then releases configuration scratch, graph and physical discovery before replaying
remaining bodies. Already published records remain owned. A graph which closes
without early publication uses the shared existing complete-schedule path. No
benchmark/path-specific behavior or new experiment switch selects this boundary.

Bounded disk capture uses complete-graph scheduling. Its payload already stays within
1 MiB, while overlapping growing configuration and semantic owners increased tracked
peak by 14.9% in the measured interleaved control. Keeping the existing single-owner
transition avoids that regression. This is the existing storage policy selecting an
ownership schedule, with the same language implementation on both paths.

Late section maps prevent early commitment to section geometry. Fixed-size numeric
section recipes retain source section slots, kind and alignment until the full map set is
known. Global completion resolves them before region/placement validation and
ordinary binding/layout. No CPU parser, expression evaluator or convergence policy
is duplicated in this scheduler.

## Captures, includes and ownership

Captured source has monotonic logical offsets. Retirement records disjoint checked
intervals; fully retired chunks release payload and cannot fault back from disk.
A partially retired shared chunk stays allocated while live bytes remain. Reader
views stop at retired boundaries; pinned rows cannot retire. Physical file boundaries
seal writers, reducing shared allocation lifetimes. Descriptor movement never changes
logical handles.

An active include suspends its parent source cursor and preserves origin, expansion
and declaration context. During interleaved resident preparation, its acquired
extent retires only after replay, before the parent resumes. Complete-graph paths
keep include captures until final capture cleanup. Templates retain their own binary
bodies/defaults, not borrowed capture pointers. Inactive branches and unexpanded templates do not acquire files. Discovery
paths required by later active includes survive physical discovery cleanup.

Resident and disk-backed capture use this same ownership protocol. Disk backing
still has a bounded resident payload budget and one private append-only scratch file;
retirement reduces RAM ownership, not the file's accumulated length. Use a genuinely
disk-backed volume rather than assuming `T:` saves RAM. Failure cleanup owns both
sessions and every partial allocation.

## Prepared module storage

Checked module spans own dense ranges in a shared prepared-record store; module
boundaries need not waste page space. Disk mode keeps at most four 32 KiB payload
pages, with directory metadata outside that budget. Records remain whole, dirty
pages persist before eviction and pinned pointers cannot move. Logical module-order
views translate offsets and cap reads at module boundaries without reloading all
records. Buffer adapters preserve the contiguous reference path.

Global completion runs its existing layout, output, binding and record-remapping
phases across pages in the original order. Section state spans batches. Metadata-only
ordering builds origins and module starts. Reachability uses the same selector through
views and retains whole-block decisions separately from lexical scratch. Runtime
bindings, typed values, parameters and member IDs transfer before frontend retirement.
The ordered contiguous stream is then rebuilt once, omission applied and backing
storage released. `beginSizing` still sees stable contiguous records. This avoids
prepared-buffer/frontend overlap; it does not avoid semantic preparation work.

Bounded prepared records previously saved 1,964,032 B / 18.72% on frozen input.
Lexical sealing saves another 1,615,656 B / 18.95%. Buffer reuse then removes
88.41% of allocation/free traffic without changing that heap peak. The remaining
runtime cost and qualification limits are documented below.
The [resource results](native-performance-diagnosis.md) separate heap, allocation
traffic and release timing. Global symbol IDs remain a temporary in-build bridge;
this storage cannot yet be exported as a reusable module object.

## What remains eager and resident

The configuration pass still runs shared lexical/configuration processing; this is
not the proposed cheap structural-only scan. Every applicable module body still gets
prepared. Exported definitions/templates, module metadata and remaining semantic owners still
grow across modules. The disk candidate now spills completed private lexical ranges;
late dependencies remain readable through owned copies. Prepared records use bounded backing with disk policy, while resident
policy retains its contiguous buffer. Binding proxies, member
identities and placement-sensitive recipes are finalized across the complete build.
Publication does not skip preparation of unreachable source or emit final bytes early.

Consequently the useful memory bound is shared runtime/packages plus simultaneous
configuration and semantic state, retained prepared data, active module/include
capture, discovery and allocation overlap. It no longer requires every completed
module's raw capture to remain resident. The measured largest overlap, rather than
a speculative object size, determines the current peak. Expanded-memory results do not establish 8 MiB fit.

## First-reference module assembly proposal

Design proposal, 2026-10-18; Erik subsequently authorized starting implementation.
The hardware repeatability batch continues against its unchanged snapshots.
The [budget and measurement evidence](native-performance-diagnosis.md) remains
the reference. This section describes the endpoint, not completed functionality.
Persistent `.ofm` reuse, fingerprints and cross-build invalidation are later
decisions; the proposed first boundary is an in-build module image.

### Intended outcome

On the first actual use of a configured module, process its body as one assembly
batch, save the result sequentially, then release its private capture, lexical and
prepared-record owners. Subsequent references use the saved module image and its
directory, without reopening source or restoring private lexical pages.

"Assembled" means fixed code/data plus the smallest remaining binding and layout
recipes. It does not mean final addresses or permanently chosen instruction sizes.
Processing an entire module is distinct from including its entire output: final
selection still retains only referenced whole `.block` units, plus ordinary
unconditional content required by current language semantics.

The expected benefit comes from replacing repeated access to source-oriented
working data with sequential writes and selected-fragment reads. This must release
owners before the old peak, rather than produce another representation alongside
them after global completion.

### Activation and language behavior

The root is active. An applicable executed reference to an imported symbol, typed
value, macro or segment requests its configured module. A mention in an inactive
branch or an unexpanded template does not. Merely discovering `.use` establishes
the interface/configuration relationship; it does not make every block reachable.
Existing mandatory module-level effects and validation remain scheduled even when
they are not triggered by a code reference. This is not permission to skip them.

Keep interface discovery separate from body assembly. Discovery must provide the
exports, declarations and structural boundaries needed to resolve requests without
first binding every instruction. Shared VM grammar owns this scan; no second
handwritten parser or CPU-specific shortcut is introduced. Exported values whose
meaning requires execution can themselves activate the module.

Configuration is fixed before body processing. Evaluate `.use ... with (...)` in
the importing context under the existing compile-time rules; preserve typed values,
schema identity and the existing equal-configuration checks for repeated imports.
First-reference order must not change module ordering, section placement, late
section maps, diagnostics or mutable/source-order observations. Processing order
and final output order are separate. Context-dependent observations become explicit
inputs or retained recipes; they cannot be frozen using whichever caller ran first.

Assemble all safely processable blocks during the module batch, indexing each
separately. Later references can select another saved block and follow its dependency
edges without repeating module preparation. Do not execute inactive file operations
or introduce errors/effects from unreachable code that the current implementation
does not execute. Conversely, do not suppress existing frontend validation merely
because no block is selected. Guarded operations that cannot yet be executed safely
remain binary recipes, not arbitrarily evaluated bytes. This limits the promise of
"once": stable work runs once; unresolved semantic work may run during layout.

Internal labels never divide reachability units: referencing a block's leading
label or a qualified symbol within it retains the entire block and its fallthrough.
Newly selected blocks can discover dependencies in other modules; iterate this
closure without rewriting already saved module payloads. Macros and segments retain
binary templates/defaults and declaration context, including numeric member and
provenance bindings. Expansion effects occur only for actual invocations.

### Saved image and ownership boundary

Use the same representation in memory and on disk. The public spelling `.ofm`
does not yet define a stable file format. Store checked base-relative or nested
region-relative offsets and module-local numeric identities, never process pointers
or build-global symbol IDs. Relocation of storage, import identity resolution and
relocation of emitted addresses are separate operations.

| Part | Retained information |
|---|---|
| Module directory | Configuration identity, exports, block/section offsets, lengths and dependency edges. |
| Fixed fragments | Raw instruction/data bytes and explicit alignment/reservation descriptors. |
| Relocations | Fragment/field location, width, addend, target identity and the required VM/package expression recipe. |
| Deferred layout | Conditional trees, loop bodies and encoding alternatives whose dependencies can still change. |
| Compile-time interface | Owned typed values, schemas, binary templates and their declaration context/dependency closure. |
| Provenance | Compact origins and diagnostic/listing information needed by enabled outputs; no borrowed source pointer. |

Retain the applicable package/CPU/capability context with each affected region,
including context changes inside a module. Decide compact field widths, byte order
and overflow handling from actual ranges before implementing the container; they
are not fixed by this proposal. Sequential writing can use bounded chunks rather
than an allocation as large as the complete module image.

Every fragment supports relocation. A temporary known value is not a reason to
discard a dependency that can change after placement. Final layout uses the shared
convergence rules to select branches, evaluate loops and relax encodings. Keep
branch/body recipes instead of enumerating combinations or all loop iterations.
Record which fragments depend on changed symbols/geometry so reevaluation does
not require reading unrelated module payloads. All CPU semantics remain owned by
packages and their established execution boundaries; each VM keeps its consolidated
v1 contract, without native version dispatch.

Publish only after writing and validating the complete module image and directory.
Then retire owners only when every later consumer's data is owned by the image or
its retained interface. Failure leaves the module failed, never falsely sealed;
normal cleanup owns partial files and allocations. Reads use bounded views and
checked lengths. Saved bytes remain immutable; layout selections/address tables
are separate so iterative placement does not rewrite disk payloads.

Use a work queue, not a stack of fully retained parent frontends. A dependency
request saves a small binary continuation and releases or spools parent working
storage before processing the dependency. Preserve existing dependency-cycle
acceptance/rejection; an in-progress module must not recurse or wait indefinitely.
Any legal cyclic reference group needs an explicit shared resolution frontier whose
maximum live working set is measured. A single oversized module must also have a
bounded spill path or a clear memory failure; module-at-a-time is not a memory bound
by itself.

### Expected memory benefit and budget

The confirmed machine budget is 8 MiB, with 6.5 MiB for the entire process and
1.5 MiB for AmigaOS. The current release linked image (764,484 B, embedded package
included) plus 65,536 B stack leaves **5,985,724 B** for heap and other process
allocations. Recalculate that allowance if the implementation changes image size.

The expanded-memory frozen controls measured 18,831,968 B peak tracked heap for
resident storage and 6,909,720 B for disk storage. Assembly live heap was
4,943,360 B. These are measured reference costs, not a census of bytes this design
can delete. The disk peak exceeds the present heap/other allowance by 923,996 B;
fragmentation and untracked allocations require further headroom. The existing
real 8 MiB full-build checks fail. Neither hardware timing nor a projection proves
fit.

Model the proposed peak as the maximum, across preparation and final layout, of:

`shared runtime heap + interfaces/directory + active module working set + bounded I/O + deferred recipes/layout state + allocation overlap`.

Count the executable/package and stack separately as above. Do not keep a resident
collection of module images alongside a rebuilt full prepared-record stream: the
final consumer must use selected fragment views or bounded streaming. Preserve
stable/no-movement execution frontiers; any required change to `beginSizing`'s
current contiguous-input contract must be an explicit part of implementation design.

The expected saving is the retired private state of completed modules, minus the
new image directories, recipes and staging buffers. It cannot yet be given as a
reliable byte forecast: largest active-module capacity, interface closure and recipe
volume are not measured. The 708,216-byte final executable is not an estimate of
module-image size; images may include unselected blocks and substantial metadata.

Use **5 MiB peak tracked heap as an initial whole-build design goal**, rather than
calling it an expected result. Against the disk reference that requires at least
1,666,840 B (24.1%) saving; against resident, 13,589,088 B (72.2%). With today's
image/stack it leaves 742,844 B inside the process budget for untracked allocations
and headroom. It is still subject to actual contiguous availability and a completed
exact 8 MiB run. Assembly's measured 4.71 MiB live heap is close to this goal;
retiring preparation owners cannot be assumed to lower that separate live cost.

### Expected runtime benefit and evidence limits

On the identical frozen hardware workload, resident runs so far take 123 and
124 s; the first disk run takes 1,292 s. Each produces the exact 708,216-byte live
Rust oracle. This is preliminary evidence while the batch continues, not a final
repeatability result. A6000 RAM is unrestricted and proves no 8 MiB fit. The first
pair's disk excess is 1,169 s; that is the opportunity ceiling for eliminating
the storage-policy penalty, not time attributed specifically to lexical reads.
The two policies also differ in scheduling and backing of capture/prepared records.

Expect fewer small random reads/seeks, dirty rewrites and repeat lexical/binding
visits. Fixed code/data should be written once and read in selected-fragment order.
Required tokenization/VM execution, dependency discovery and placement-sensitive
work remain. Assembling every block of an activated module can do more work than
assembling only its ultimately selected blocks; interface/template retention and
fragment processing can also add cost. No speedup over resident is promised.

An initial runtime goal is no more than **2 times the completed resident median**
on identical hardware/input, alongside memory fit. Using the first 123 s run only
as an illustration gives 246 s, about 81% below 1,292 s; this is an acceptance
ambition, not a forecast. The eventual three-run median supplies the baseline.
At least three release timings per configuration are required for a performance
claim. Measure release runtime separately from instrumented memory, using existing
telemetry and the already authorized narrow I/O counters. Do not add an observer
framework to pursue this proposal.

### First native component: publication and reuse

`binary_module_image.asm` implements a checked immutable image: a 28-byte header,
12 bytes per block directory row, 16 bytes per absolute BE32 relocation and raw
payload. Only offsets and numeric identities are serialized. Canonical sorted
directories give linear validation and binary block/relocation lookup. Validated
readers avoid repeating whole-image validation for each selected block. Copies use
native long transfers with byte tails; unaligned fields are valid on the native
68020. Signed addends, other widths/endian forms and layout recipes are not yet
represented. They must remain outside this restricted boundary until supported.

`binary_module_request.asm` owns one compile/build callback on the first requested
block, validates publication and reuses it for subsequent block requests. Failed
publication releases partial ownership and remains failed until explicit release.
Recursive acquire/release requests latch failure without freeing the outer
compiler's live allocation. A missing block preserves an already published image.
Returned views borrow that immutable image until release.

The standalone `module_image_probe.asm` builds from instruction/data bytes freshly
assembled by Rust, moves the image, poisons and frees its source/original buffers,
then links two copies with different imported addresses and selects another saved
block. A third block remains unselected. Its 20-byte artifact matches the live Rust
assembly exactly under the real 8 MiB Shell profile. The harness also verifies one
builder invocation, corrupt/truncated input rejection, bounds/overflow and resolver
failure before output mutation, sticky partial failure, recursive ownership and
idempotent cleanup. Caller buffers remain nonoverlapping and the link resolver
must be pure/stable for the two-pass validation/emission contract.

This is a native **component proof**, not native compilation of a source module:
the callback packages supplied bytes; it does not yet run the native frontend or
encoder. The CLI does not call these components, and no module is prepared early
or spilled to disk through them. All images in this component proof remain resident.
There is no full self-host through this approach, no full-build resource reduction
and no 8 MiB fit claim. The current engine remains the reference, with no new
production switch. Existing telemetry/build behavior is unchanged.

Validation also exposed a Rust M68K package selector bug for symbolic MOVE.L
immediates with addends and memory destinations. The separate repair uses the
existing relocation program and has exact opcode/addend/BSS-relocation regression
coverage. It is not a module storage optimization.

### Next coherent integration slice

Start with a configured leaf module containing several named blocks, internal
forward references, a retained exported value/template, and relocations to an
importer's later-placed section. Select two blocks at different points to prove
that late references need no source replay, and leave another block unselected.
Produce the saved image before global completion, release that module's private
owners, then consume it through the normal shared final-layout/output path.
This tests the necessary ownership boundary rather than a final-only serializer.

Keep the current resident/disk engine as the reference; no default change or stack
of experiment switches. Qualify shifted placement, internal-label fallthrough,
configuration, guarded errors, inactive includes and macro declaration context
against live Rust. Then cover placement-dependent recipes, dependencies and larger
modules before extending use broadly. A restricted first slice must route unsupported
forms explicitly through the reference for the entire affected module, or reject
the experimental request clearly; it must not silently freeze them or reject valid
input from the maintained path. This first slice alone makes no whole-build memory
or speed claim.

Integration needs the frozen full self-host plus maintained multi-module and
macro-heavy controls, exact unchanged-input Rust output, tracked cleanup zero and
no allocation failure. Use deterministic retired-capacity and I/O counters first;
stop if directory/recipe retention or suspended modules simply recreates the old
peak. Whole-build acceptance requires meaningful measured saving, release timing,
and a fresh exact full self-host under the real 8 MiB profile. The saved-image
component above is a prerequisite, not completion of this integration slice.

## Sequential lexical storage sealing: current checkpoint

The capacity census identified 50,815 entries, 38,298 unique names and 1,791,485
name bytes over 220 module scopes. Near the old peak, entries, module arrays and
name pages occupied 4,325,888 B. The planning target was at least 1 MiB of measured
whole-build saving, not a guarantee from adding up final retained objects.

The implemented candidate preserves global IDs through an eight-byte bridge and
moves private entry/name payloads behind a four-page 128 KiB cache. Successful
publication seals the newly appended range and frees its active arenas. Interface
facets remain retained. Global completion still reads or mutates private dependencies
through checked owned copies; no raw arena/cache pointer escapes. Names retire at
the established post-binding numeric boundary, before later entry consumers retire.
Resident mode keeps contiguous storage through those same access interfaces.

Module owner/origin/visibility arrays remain resident: 393,216 B at full capacity.
The bridge needs up to 524,280 B for 65,535 identities. Retained typed values,
schemas, templates, imports and plans are unchanged. Closing a module does not
justify deleting its private dependency closure, and this checkpoint does not do so.
Failures latch and flow to frontend publication/cleanup; partial owners release normally.
The existing `--capture-dir` policy selects backing, with no additional selector.

The unchanged frozen sealing comparison reduces peak from 8,525,376 to
6,909,720 B: **1.541 MiB / 18.95% saved**, above the 1 MiB target. The binding repair
leaves heap/work counters unchanged. Reusing four existing 32 KiB cache buffers
then reduces allocated/freed capacity from 826,338,176 to 95,775,616 B, with the
same peak, exact live Rust output and zero cleanup ownership. Instrumented
preparation falls 289.94 to 166.42 s. Separate release timing and smaller controls
are in the [measurement report](native-performance-diagnosis.md).

The current release executable also assembles its entire current source exactly in
both resident and disk modes. The additional instrumented current-source case
does not complete within
1,200 s, after the ORDER marker and before BIND. Do not use that timeout as a peak
measurement or infer it was introduced by cache reuse: an equivalent pre-reuse
instrumented current-source control has not been run. Focused native component and
module checks pass. Keep the contiguous path and preserved pre-sealing executable
as references; review the anomaly before further ownership changes.

Buffer reuse is implemented, with no extra state or larger cache. It transfers
allocation ownership after flushing an unpinned victim, preserving logical metadata;
short views expose only initialized bytes. Dirty writes, pins and failed-I/O cleanup
have fresh native component coverage. The static capacity census remains a forecast,
while the completed frozen MEMD run establishes the actual peak saving.

A resident chain/hash bridge remains a proposal for avoiding cold payload acquisition
during rehash/nonmatching lookup. A 14-byte bridge adds 393,210 B at maximum count,
projecting about 1.166 MiB remaining sealing saving at an unchanged peak frontier.
It is unimplemented and follows the ordered work in the current plan. No 8 MiB
fit is established by these expanded-memory runs: frozen tracked heap
alone is 6.59 MiB, and prepared/assembly live heap remains 2.83 / 4.71 MiB.

Semantic object-local identities, explicit remapping and native early lowering
remain unimplemented integration work for the newly authorized approach above.
The saved-image component alone does not provide them. Do not start another
macro-only investigation or a final-only cache.
The census and current receipts remain outside `target` under
`/tmp/opforge-module-capacity-20261018`, `/tmp/opforge-module-sealing-20261018`
and `/tmp/opforge-cache-reuse-20261018`.
Git preserves the superseded forecast and experiment chronology.
