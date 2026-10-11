# Resumable native source execution

Status: named preparation-known acquisition and the private frozen-input assembly
lifecycle have focused native proof. Growing-source execution remains a design
target.
The [completion plan](native-runtime-reset.md) records current evidence and completion order.

The latest frozen BS37/reservation opcore audit completed all 86 roots: 29
positive roots matched requested Rust artifacts exactly, 36 expected negatives
rejected with diagnostics (without exact diagnostic parity), and 21 positive
roots exited 20. No pass/rejection status changed from the BS36/schema audit;
`syntax.asm` advances from its reservation on line 114 to a labeled conditional
on line 280. Its 371.93-second batch includes harness overhead and is not
performance evidence. This corpus result does not qualify growing-source
acquisition, live-PC/placement controls or dynamic binding ownership; those remain
separate focused proof requirements.

## Responsibility boundary

Active acquisition must use real assembly state when its guards depend on the
program counter, selected instruction sizes or section placement. Preparation's
known-value evaluator must not become another assembler. Keep the existing
shared expression compiler, statement dispatcher, data emission, encoders,
repetition snapshots and section/mapping owners authoritative.

`binary_source_cursor.i::drainPrepared` already provides a useful boundary:
publish a complete numeric record, execute it, then resume source acquisition.
Retained numeric captures supply replay; original text remains tokenizer and
diagnostic input. Acquired bodies that introduce module/use graphs remain a
separate gap until graph scheduling is explicitly supported.

## Assembly session and stable publication

Extract the existing assembly lifecycle into `beginSizing`, `advance`,
`finishSizing`, `emit` and `release`. `advance` suspends only between complete
records at the published frontier. An unfinished control envelope means more
source is required, not successful end of assembly. Finish with the existing
bounded convergence and strict emission checks; remove the superseded traversal
after callers migrate.

Store cursors, loop headers/bodies and inventory references as offsets. Sessions
borrow allocation owners and refresh interior views after growth. Never leave
an expression VM or encoder suspended across record relocation.

The first lifecycle checkpoint pins the full record allocation and assembly
context, including count, value/defined/section buffers, typed owner and member
keys. It stores the traversal cursor as an offset and permits record-budget
yield/resume only against the complete frozen frontier. It rejects a changed
base, partial frontier or relocated context before using borrowed views. This
checkpoint does not support incremental publication or relocation; the existing
branch, repetition and inventory views must migrate before either is allowed.

The sole `assemble` driver now uses these lifecycle APIs; the temporary reference
entry and mode are removed. Twelve fresh collection cases exercise one-record
yield/resume, failed initialization cleanup, premature sealing, invalid
base/frontier, rejection without stealing an active owner and repeated release.
Twelve composed-binding positives and two malformed-name rejections also pass
on 68020 and 6502. The isolated unchanged release control adds 0.76% on one
sample and 952 bytes of linked static storage; peak heap and a current full
self-host are unmeasured. These checks do not qualify actual-state acquisition.

`binary_symbol_storage` now owns Values, Defined, SectionIds and canonical
member keys independently. Growth stages only required replacement blocks,
including an allocated typed-kinds table. It stages allocations and copies before
committing views and Count; failure releases staged blocks and preserves the
published context. Existing cells survive and only appended bytes are cleared.
Kinds remain lazy when absent. Preparation-exported keys and kinds can precede
the first scalar allocation. Count remains bounded at 65,536. A caller-provided
reserve callback follows the normal block allocation ABI and permits component
failure injection; production uses bounded geometric block growth.

The CLI uses this owner, but frozen sessions still reject changed Count or views.
Growing source publication is not enabled. The callable component probe covers
small initial allocations, each of five allocation failures with unchanged
published cells/pointers/counts, prepared keys/kinds, lazy kinds, unchanged and
shrinking counts, successful maximum-count growth and repeated release. Fresh
component and production matrices each pass twelve exact positive cases and two
expected malformed-input rejections. The unchanged release control takes
27.064863 seconds before and 26.944213 after (-0.45%, one sample); linked static
reservation grows by 568 bytes. Peak heap and current full self-host are
unmeasured. Preparation retention and incremental inventory/binding publication
remain necessary before actual-state acquisition can use these growing buffers.

Retained repetition headers and bodies now use offsets, including the pending
header. The PRVM operand was already header-relative. Guard declarations are
reacquired through the stable inventory owner; transient dependency frames are
reconstructed per call. Resume refreshes borrowed views after the existing
frozen-frame checks. Active header spans and body/frontier limits are checked
before publication. The isolated BS35 probe passes twelve fresh collection cases
on both packages, including nested rebasing, truncated headers, overflow/null
bases, unchanged views after rejected refresh and one-record lifecycle traversal.
The production candidate host-assembles at 663,456 image bytes and 688,164
linked static bytes (504 more than the matching storage checkpoint). Its separate
native matrix passes twelve exact positives and two expected rejections on both
packages. The unchanged release control takes 26.848584 seconds before and
27.116166 after (+1.00%, one sample). Peak heap remains unmeasured. No incremental
inventory or source acquisition is enabled, and the public session still rejects
a changed record allocation.

Separate incremental binding publication from final source sealing. Already
consumed references must not be redirected by later import/ancestor rewriting.
Use stable occurrence/proxy identities and existing numeric binding plans;
publish deltas rather than copying entire tables per statement. Final sealing
checks unresolved references. Source controls compile through the shared grammar
and evaluate against the actual assembly Context, without mirroring values into
the preparation evaluator or inventing a provisional PC.

Inventories need begin/append/seal operations. Newly acquired declarations belong
to the current lexical owner. Repetition frames must enroll a newly published
declaration and save its enclosing value once; avoid repeatedly reallocating a
rectangular table for all existing iterations.

## Named acquired collections

Named `.bfor` and `.bwhile` results are typed collections of iteration address
records, including empty collections. Materialized records must preserve that
meaning, not replace a named collection with an ordinary label.

Use private numeric collection/iteration boundary records owned by the prepared
source layout. Centralize their named fields, actions and bounds validation.
Inventory and repetition/snapshot owners consume them; source control schedules
them without implementing another collection system. Publish each bounded record
batch once. Already expanded bodies must not be repeated again during assembly.
Preparation-known acquisition now uses these boundaries. Twelve fresh exact
controls cover counted/list/while/empty/nested collections, macro/segment includes
and colonless/explicit-colon labels on both 68020 and 6502 packages. Inventory and
runtime independently validate nesting, labels, iteration indices and balanced
boundaries. Initializers compile into full scratch storage before a bounded batch
copy. Actual assembly-dependent guards still require the session above.

## PC and replay semantics

Keep emission PC separate from the previous ordinary statement's start. Reuse
existing repetition observation and `.org` behavior. Acquisition callbacks and
private markers must not overwrite that observation. Current Rust rejects `*`
as the loop PC expression; pin `$` behavior with live oracles rather than adding
syntax.

Retain a compact numeric acquisition transcript containing source identity,
invocation/iteration path, compiled guard, selected outcome and materialized span.
Later sizing/emission must agree with it. Changed traversal is an instability
error. Capture selected file contents once per acquisition occurrence; replay
must not reread mutable host files. Preserve strict unavailable-value rejection
and short-circuiting of unevaluated operands.

## Mapped and placed sections

Establish flat source-order execution first. Source order is the graph-ordered
module schedule, not file discovery order. Then replace filtered semantic sweeps
with one source-order traversal per sizing round and section-owned cursors.
Output routing chooses storage, not whether language effects execute.

Reuse mapping freeze/check/merge, placement, section bounds and relocation owners.
Preserve section provenance through values and repetition records. A provisional
numeric address is not automatically absolute. Mapping changes require another
bounded complete sizing round, never prefix reassembly after each statement.
The focused mapped-loop capability rejections are removed after positive fresh
native proof; the mapped-flat repair results follow.

The focused mapped-flat output repair now passes all ten former mapped owner
cases freshly and exactly on 6502 and 68020. Seven mapped Hunk controls are also
exact, covering reversed output selection, dependency-first assembly and two
logical destinations; Hunk BSS instruction relocation matches exactly. The
implementation resolves slots from actual geometry, orders concrete first
declarations, tracks logical alias destinations, selects contiguous flat output,
and keeps BSS `initializedLimit` separate from `size`. Map v2/v3 are byte-identical;
the v3 image is 678,120 bytes and linked reservation 711,228 bytes (+260 versus
activity v7), with no state memory growth. This focused result does not establish
a fresh full self-host or 2 MiB fit.

The separate 24-case `.ds` matrix passes with 12 exact artifacts and 12 expected
rejections across both CPUs. The unchanged `syntax.asm` still rejects at labeled
`.if` line 280; this is not a full corpus qualification.

## Implementation and evidence

1. Preserve the focused composed-binding and named-acquisition checkpoints;
   dynamic physical binding ownership remains a source-session gap.
2. Extract lifecycle and offset traversal with unchanged frozen-input behavior.
   Qualify budgeted yield/resume, invalid base/frontier, premature sealing and
   failure cleanup, then compare the complete CLI with the preserved reference.
3. Publish actual assembly context while retaining frontend/captures until seal.
   Force small allocation capacities to exercise growth with live nested controls.
   Qualify flat strict acquisition. First pin Rust for `.org 0`, `.while $<2`,
   `.include "one.inc"`, `.endwhile`, with `.byte 7` in the leaf. The expected
   statement-start result is three bytes; the live oracle must establish it.
   Add false missing-file, `.org`, alignment and package-encoded-size controls.
4. Cover named acquired snapshots, nested macros/includes and forced relocation;
   validate unavailable or changed activity diagnostics.
5. Migrate mapped/placed controls to section-owned source-order traversal.
6. Requalify shared corpus, engineering guards and full current-source self-host
   before claiming opcore completion and moving through the CPU families.

Keep explicit existing limits: nesting 16, iterations 65,536, record storage
2 MiB and ordinary owned blocks 1 MiB. Check overflow before allocation. Measure
release time separately from telemetry. Instrument published records, cursor
resumes, sizing rounds, captures and peak owned bytes through existing macros.
Retained captures and live context overlap until sealing, so heap growth needs
actual measurement; image reservation alone cannot establish peak memory.

### Text integration checkpoint

The BS36 text checkpoint added validated spans through the shared numeric text
grammar and text registry. TextPlan remains at byte 228 and TextPlanBytes at byte
232; the current BS37 header extends this layout to 238 bytes with the DS directive
ID. Only current packages are supported. Preparation supplies global ASCII/PETSCII source-origin
IDs, and encoding names are normalized once into global numeric IDs. Assembly
handles literal, macro and parameterized text directives, plus string operands in
`.emit` byte/word forms. Definitions rebuild on complete replay, selection resets
at module entry, and the string provider grows its output buffer lazily and releases
it during cleanup. Macro preprocessing preserves UTF-8 characters; the Rust
controls include Unicode whitespace around encoding names.

Fresh native checks complete 52 controls across 68020 and 6502: 24 positive
artifacts match their live Rust oracles exactly; 28 invalid inputs reject with
fresh completion and a diagnostic (not exact diagnostic parity). They cover
registry replay, forward definition values, nested definitions and module closure,
scalar/aggregate emission, escapes, active branches, normalized names and macro
substitution. Separate component checks cover decoded-string offsets and bounds,
the shared text grammar, and assembly-session suspend/release/restart cleanup.

For this text integration alone, the unchanged 11,592-byte imported-macro control
on FS-UAE `68020-10m` takes 27.420387375 s before and 33.112640750 s after
(+20.8%, one release pair without telemetry). Linked storage grows from 690,024
to 703,488 bytes (+13,464, +2.0%); executable size is 670,280 bytes. Separate
instrumented before/after runs both peak at 1,282,280 owned bytes and finish at
zero live bytes, with all allocations freed. This peak excludes linked storage;
it does not establish the 2 MiB product target. The text dispatch cost remains
visible for later optimization. These focused results do not establish complete
opcore parity or a new full current-source self-host.

### Conditional binding ownership

The Rust conditional ownership repair is committed as `aaca8086`; 98 focused tests
pass. Each Rust layout pass rebuilds executed owners, references, reachability and
schema uses. Unresolved longer `.const` candidates may defer initial evaluation;
they grant no executed visibility or immutable certificate. Native adds recursive
shared-EXVM certification and evaluation for an
immutable scalar closure held in private arrays. It does not publish those values
to global symbols. Lazy records retain 18 bytes per source ID plus boundaries.
Four focused fresh positives match exactly across both CPUs, including transitive
references and sign handling. The v3 feature control assembles
384 source bytes into 268 packed bytes in 1.801896042 s of release execution.
Separate MEMD telemetry reports a 641,792-byte peak, 1,031,784 bytes allocated and
freed, and zero live bytes after cleanup.

The current execution-activity repair separates immutable declaration ownership
from value availability. Ordinary-loop candidates stay out of eager dependency
indexing. Each completed traversal records executed owners; only a proven inactive
longer owner permits shorter member fallback. Unknown owners defer, and active
scalar or unavailable owners retain ownership. Lazy numeric scalar observations
survive sizing resets independently of compound descriptor arenas. Final emission
remains strict; no compound pointer or immutable certificate is restored from an
observation.

The activity checkpoint's 112-case matrix completed across 68020 and 6502: 62
exact positive artifacts, 40 expected diagnostic rejections and ten mapped
section failures. The counts are historical; that matrix was not rerun after the
mapped-flat repair. Expected rejections do not qualify exact diagnostic text.
The 523.12-second batch duration is correctness-only.
Rust commit `7073f2de` also removes readonly source symbols that become inactive
following sizing convergence while retaining incoming configuration and schema
metadata. This corrects a target-dependent stale-label result.

The unchanged release control takes 33.291479875 s versus the BS37 baseline's
32.955889917 s (+1.02%, one sample per state). The release image is 677,860 bytes,
linked reservation 710,968 bytes (+3,092 and +2,960). Telemetry macros add no bytes
to that release image. Separate telemetry reports an unchanged-control peak of
1,282,360 bytes (+16), with zero live bytes after cleanup. The 356-byte activity
control exercises the lazy maps and scalar rows, peaks at 690,392 bytes and frees
all 1,079,392 allocated bytes. Its counters report two passes with observations,
zero changed rows and one restored row. See the
[separate measurements](../performance/vm-efficiency.md#executed-declaration-activity-and-scalar-observations).
These `68020-10m` checks do not qualify the 2 MiB product budget or a new full
current-source self-host.

Schema replay metadata remains independent of pruned block code: only eligible
established schema seeds can be demand-copied. Retained metadata cannot publish
an inactive declaration. Cross-physical-block conditional selection and forward
variable-width repetition/reservation layout still require further qualification.

The earlier schema-replay-only checkpoint measured 32.804228 s against 33.112641 s
(-0.9%, one comparison), with 564 more linked bytes; it predates this conditional
repair and is not its performance result. Full opcore parity and a fresh full
current-source self-host remain pending; BS35 is the latest qualified self-host.

Rust forward named iteration records now retain observations from executed
immutable owners independently of dependency-graph certification. Ordinary
contextual typed aliases still require their existing certificate; inactive or
replaced owners cannot retain a stale record. Forward module import qualifiers
can defer incomplete scalar-prefix lookup during collection, with strict final
resolution. The older scalar-prefix fallback oracle now expects rejection from
its executed longer owner. Focused host coverage passes; native forward-record
parity is a separate pending positive case.

### Shared reservation checkpoint

BS37 adds numeric `.ds` binding to shared expression preparation and the assembly
output owner. Flat internal gaps are deferred until the next initialized emission;
leading/trailing reservations create no payload or record spans. BSS sections reuse
the existing independent fragment placement and reservation lifecycle, including
two sections in one region. Artifact policy distinguishes explicit Hunk descriptors
from this layout machinery, so flat outputs retain initialized payload and addresses.
The 24-case native matrix passes on 68020/6502 (12 exact positives, 12 expected
rejections); six final Bin/Hex/S-record controls and the prior Hunk BSS regression
match live Rust. The complete original syntax root remains incomplete at its
labeled conditional on line 280. [Separate timing and memory measurements](../performance/vm-efficiency.md#shared-byte-reservations-bs37)
show no material unchanged-path regression. The expanded ownership matrix and full current-source self-host
qualification have separate coverage; no full opcore claim is made.
