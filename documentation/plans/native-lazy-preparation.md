# Lazy preparation: inventory, publication and lifetime

Status: opt-in first-use macro-body plan binding is measured and natively qualified,
2026-10-15. The preceding publication/lifetime, whole-unit work and template
inventories are measured. The ordinary eager path remains the reference;
no capture is retired early. Historical checkpoint details are in Git history.

## Active experiment: first-use macro-body plan binding

Authorized 2026-10-15. Hypothesis: keeping declaration-time numeric binding while
postponing captured descriptor copy/remapping avoids the measured never-invoked
macro work and reduces its plan arena, at an acceptable cost for compact recipes.
The compile-time `OPFORGE_DEFERRED_MACRO_PLANS` switch keeps the eager reference.
The shared `experimental/build_configuration.i` maps that Rust preprocessor
definition to the module-local `DEFER_MACRO_PLANS` constant (otherwise zero).
Owners use ordinary `.if` conditions; optional ABI fields reserve zero bytes when
disabled. A native source build can select the same mode by defining that constant
in the shared include, without adding native CLI define support.

The first slice covers ordinary captured macros. The writer still binds names and
produces their packed map at declaration. A recipe owns that map and numeric global
capture offset/candidate handle; it contains no borrowed pointers. A direct,
cursor-independent resolver reacquires an immutable capture view across includes
and region movement. First invocation binds all body plans once, caches normal
handles and then queues the macro. Header/default binding, invocation scope and
shared VM grammar stay with their existing owners. Segment/statement templates
and non-captured input stay eager. Captures already survive through replay;
this experiment does not retire them early.

Malformed-plan validation remains eager through shared validators. Compare the
new shared validation's eager path with the preceding eager executable as well
as comparing eager/first-use builds from the same final source. No behavior is
selected by telemetry source name, fixture, expected output or benchmark identity.

Success requires fresh exact Rust and eager-output parity, owned-map/relocation/
cache/invalid-input/preservation probes, and separately measured release runtime
and heap/linked memory on identical frozen inputs. Report actually created/bound
recipes and storage; do not count every retained header/recipe as removable. Then
run a separate full current-source self-host of the selected configuration.
Stop or revise on semantic/diagnostic drift or a material runtime/memory regression.

This postpones descriptor copy/remapping, not lexical name binding, tokenization,
capture construction or all expression preparation. The existing declaration
writer still establishes numeric identities. Recipe storage is a 20-byte header
plus two bytes per packed-map entry (including its sentinel). Closing records can
also create recipes that are never retained in a body; their overhead is included.
All body alternatives are bound together on first invocation. Smaller first-use
units and early capture retirement remain separate experiments.

### First-use memory comparison

Both fresh instrumented runs assemble the same frozen 198 files / 2,614,471 source
bytes into the complete live Rust 708,592-byte output. They report zero allocation
failures, profiling errors and terminal tracked heap. These measurements exclude
OS/stack/allocator metadata and do not establish the 2 MiB product goal.

| Resource | Eager reference | First use | Change |
|---|---:|---:|---:|
| Tracked heap peak | 47,587,208 | 43,992,264 | −3,594,944 (−7.55%) |
| Allocated/freed capacities, each | 112,389,800 | 108,195,304 | −4,194,496 (−3.73%) |
| Linked reservation, memory image | 794,472 | 796,072 | +1,600 |
| Memory image file bytes | 758,636 | 760,336 | +1,700 |
| Terminal tracked heap | 0 | 0 | 0 |

The experiment creates 18,823 recipes and materializes 2,284. The 16,539 remaining
recipes reconcile with the earlier inventory's 16,351 body-binding calls in
never-invoked instances plus 188 closing records in invoked instances. They are
not 16,539 unused macros. Used recipe storage is 585,728 bytes; the resulting normal
plan arena uses 606,198 bytes. Neither is a reserved-capacity or whole-session peak
measurement. Zero entry-cache hits is expected here: `BodyReady` skips repeat body
binding at the template boundary. The component probe separately exercises cache
reuse. Captures and template bodies still remain live under their original owners.

Evidence: `/tmp/opforge-first-use-full-final-20261015/memory-reference/result.json`
and `memory-audit/result.json`; the memory runs' elapsed times are diagnostic only.

### Separate release runtime comparison

The same frozen full input and package configuration is assembled serially by
uninstrumented eager/first-use executables. Every run requires fresh native
completion, exit zero and the entire live Rust output.

| Order | Eager seconds | First-use seconds |
|---|---:|---:|
| Eager then first use | 102.714222 | 101.621366 |
| First use then eager | 101.880230 | 102.866117 |
| Mean | 102.297226 | 102.243741 |

The mean difference is **−0.053485 seconds / −0.05%**: effectively unchanged,
well within the observed spread, not evidence of a speedup. The measured value is
memory reduction. The release image grows 747,740 → 749,048 bytes (+1,308), with
linked reservation 781,512 → 782,772 bytes (+1,260). These are distinct from the
instrumented-image costs above.

All six full frozen-input runs pass. Evidence:
`/tmp/opforge-first-use-full-final-20261015/summary.json`.
The eager path remains the default; the experiment remains opt-in.
The benefit is workload-dependent: this input has many never-invoked definitions.
Used bodies retain recipe storage as well as their materialized plans. These
results do not establish a memory or timing benefit for projects that invoke
every definition, and do not justify promoting the switch globally.

### Shared-validator refactor measured separately

The preceding eager executable assembles the identical frozen input in
102.535939 and 103.142029 seconds (mean 102.838984). The new eager path's two
samples above average 102.297226: **−0.541758 seconds / −0.53%**. This shows no
observed slowdown, not a demonstrated speedup at this sample size. It is separate
from first-use binding's −0.05% comparison. The refactor adds 248 release-image
bytes and 248 linked bytes; it introduces no new heap owner in the eager path.
Both old-executable runs finish with complete live Rust equality.
Evidence: `/tmp/opforge-first-use-validation-cost-20261015/summary.json`.

### Qualification

The six new macro controls pass on the first-use memory and component-probe images
(12 fresh native runs). They cover unused/repeated calls, mutable declaration
values versus caller shadowing, defaults, included definitions after source-slice
changes, definition-relative child includes, and the existing 68020/6502 macro
language matrix. The component probe covers malformed descriptors/maps, owned-map
mutation, capture relocation, failed realization, eager descriptor equality,
cache/reset behavior and register/CCR/stack preservation. All 20 prior lifecycle,
module/include, whole-unit and template controls pass on eager and first-use
memory images (40 more fresh native comparisons).

After adding source configuration, all five rebuilt release/memory/probe images
are **byte-for-byte identical** to these measured and controlled executables.
Source identity `fnv1a64:fe289ab69d98d9c2`:
`/tmp/opforge-first-use-images-config-width-20261015/images.json`.
Identity proof:
`/tmp/opforge-first-use-tools-20261015/source-configuration-image-identity.json`.
The configured source export also matches the globally defined feature build
exactly (708,512-byte `fnv1a64:91050030ced3cf89`), preventing a disabled-feature
build from being mistaken for the experimental self-host.

The separate **full current-source self-host with first-use binding enabled**
passes: 209 files / 2,643,887 source bytes, one embedded 68020 package, no memory
instrumentation, fresh START/DONE and exit zero. The complete **708,512-byte**
native output equals both the freshly assembled Rust oracle and the actual
bootstrap executable (`fnv1a64:91050030ced3cf89`). Native START-to-DONE duration is
**107.971166 seconds**. This is its own input/configuration, not another sample of
the frozen-input optimization comparison. Source manifest:
`fnv1a64:dc77a9a564b1057c`.
Receipt: `/tmp/opforge-first-use-current-config-width-proof-20261015/result/native-result.json`.
Qualified export: `/tmp/opforge-first-use-current-config-width-20261015/manifest.json`.

The default eager configuration also completes a separate current-source
fixed-point self-host: fresh native output, fresh Rust output and its actual
bootstrap all equal the complete **707,204-byte** executable
(`fnv1a64:0c34b11b04c4bcb1`), with exit zero and fresh START/DONE in
**103.837460 seconds**. Its 209-file source capture includes two unreferenced
files; this count is not a claim that all files contribute linked code. It uses
the default build-configuration include, exercising the zero-width optional
fields on native as well as Rust. These different configured self-hosts are
correctness checks, not a matched-input performance comparison.
Receipt: `/tmp/opforge-first-use-current-eager-proof-20261015/result/native-result.json`.

FS-UAE uses the maintained existing A4000/68040 memory profile; this is not A6000
hardware or 2 MiB qualification.

Focused Rust oracles, asm Clippy with warnings denied, Rust/native formatting,
44 self-host-tool tests, 12 comparison-tool tests, five memory-decoder tests and
147 workflow tests pass. The focused redundant-test guard reports no blocking
findings in changed assembly; report-only ABI probes remain visible. A capture
summary unit test now supplies its own layout fixture rather than depending on
live conditional assembly syntax; its artifact-proof assertions and validators
are unchanged. Independent reviews found no concrete implementation defect.

## Template inventory contract

Hypothesis: a material part of definition-body binding and retained template
payload belongs to definitions that the eager preparation path never invokes.
Inventory those instances and their accepted consumers before choosing a deferred
preparation experiment. Do not change execution, skip work or release payload early.

The opt-in work observer records numeric `(session, pool, handle)` identities,
completed definition/default/spelling/body extents, accepted invocations, nested
and default consumers, and definition-context binding calls/committed bytes.
Consumer relationships group caller definition, callee definition and source origin,
with one incoming invocation-line exemplar. Lookups and preview reads are not
consumers; empty accepted invocations are. Calls in subsequently discarded blocks
still count as eager preparation consumers, not evidence of final reachability.

Four concurrent numeric owner slots preserve the configuration/replay overlap.
Each instrumented template state gains an eight-byte owner identity/source floor. Ordinary state
and execution order remain unchanged; no borrowed pointers survive a hook. Fixed
inventory limits are 2,048 registered instances and 4,096 consumer relationships.
Overflow, exhausted bounds, open definitions, unfinished owners and failed
reconciliation explicitly prevent a complete inventory claim.

Reconcile closed-instance payload and explicit orphan/shared residual against live
pool Used values before release. Body binding bytes are committed preparation
deltas; all live plan-arena bytes remain unattributed as retained storage. Do not
infer unique retained-plan ownership from those deltas. Source paths are borrowed
for optional terminal diagnostics; generated/nested records may inherit invocation
lines, so exemplars are not guaranteed original physical source coordinates.

Success requires native lifecycle/default/nested/statement/include controls,
register/CCR/stack and reset/bounds probes, unchanged ordinary images, full-input
Rust byte equality and complete conserved inventory. Measure incremental runtime
without memory telemetry against the preceding work observer, then measure heap
and linked reservation in a separate identical-input memory pair. Stop or revise
on unreliable attribution or disproportionate diagnostic overhead. There is no
deferred implementation or savings claim in this slice.

## Template inventory results

The full frozen input registers **1,361 definition instances**, all macros in replay
session 2; configuration session 1 registers none. Both sessions finish. There are
**594 accepted calls** (547 direct, 47 nested), 203 grouped consumer relationships,
and two default-consuming calls/slots. Every row is closed, every call reconciles,
and observer errors/unattributed definition binding are zero. These are session
instances, not distinct source-level macro names or final-output reachability.

| Accepted consumers in its session | Instances | Body binding calls | Committed bound-plan bytes | Template payload bytes |
|---|---:|---:|---:|---:|
| None | 1,173 | 16,351 | 2,459,016 | 556,225 |
| At least one | 188 | 2,472 | 362,812 | 84,469 |
| Total | 1,361 | 18,823 | 2,821,828 | 640,694 |

Never-invoked instances account for **86.87% of definition-body binding calls**,
**87.14% of their committed bytes**, and **86.82% of template payload**. Their calls
are only **3.84% of all 425,557 binding calls**. All belong to telemetry macro
sources, instantiated in their owning contexts:

| Definition source | Never-invoked instances | Body binding calls | Committed bound-plan bytes |
|---|---:|---:|---:|
| `memory_telemetry.i` | 835 | 12,428 | 1,899,452 |
| `telemetry_macros.i` | 213 | 2,346 | 331,760 |
| `hotspot_telemetry.i` | 53 | 628 | 88,760 |
| `capture_telemetry.i` | 39 | 522 | 75,060 |
| `binding_telemetry.i` | 33 | 427 | 63,984 |

All 640,694 template-payload bytes reconcile without residual. Never-invoked
payload comprises 215,832 definition-header bytes and 340,393 retained body bytes;
there are no default/spelling bytes in that group. Headers and binary recipes can
still be necessary for a deferred implementation. The **3,082,134 live plan-arena
bytes remain entirely unattributed as retained storage**. Committed work deltas
do not prove unique allocation ownership, removable bytes or elapsed-time savings.

### Separate identical-input runtime overhead

The final observer and preceding observer use identical audit flags/packages and
the same frozen 198-file input. Timing builds omit memory/reporting telemetry.
All six full-input comparisons complete with fresh START/DONE, exit zero and full
708,592-byte live Rust equality. The final memory pair also requires complete
inventory and conserved work accounting.

| Serial pair | Previous observer seconds | Template inventory seconds |
|---|---:|---:|
| Reference then inventory | 108.424515 | 107.154250 |
| Inventory then reference | 108.366648 | 107.618555 |
| Mean | 108.395581 | 107.386402 |

The measured difference is **−1.009179 seconds / −0.93%**. There is no observed
slowdown in these final samples; the small sample and variation across attempts
make this insufficient evidence of a speedup. No preparation work was removed.
The earlier capacity-exhausted observer attempt measured +1.10%; it is not the
qualified final inventory or another optimization result.

### Separate identical-input memory overhead

| Resource | Previous observer | Template inventory | Increase |
|---|---:|---:|---:|
| Tracked heap peak | 48,390,024 | 48,390,040 | 16 |
| Allocated/freed capacities, each | 113,993,896 | 113,993,912 | 16 |
| Linked reservation, memory image | 1,814,960 | 2,213,048 | 398,088 |
| Terminal tracked heap | 0 | 0 | 0 |

Both fresh memory runs exit zero with full live Rust equality and no profiling
errors/allocation failures. All 18 original global work totals are identical.
The inventory's fixed tables account for 394,440 bytes; linked overhead also
includes observer code/diagnostics. This is opt-in diagnostic cost, not a release
memory regression; linked plus tracked heap excludes OS/stack/allocator metadata.

The initial 1,024-row attempt exhausted capacity and was **not** a complete
inventory despite successful assembly. The final limit is 2,048; definition,
consumer and path report bands are disjoint. The runner now rejects incomplete
inventory explicitly. A 1,361-row fixture crosses the old limit and phase boundary.

### Qualification and evidence

Forty final fresh native control runs cover all 20 controls on the repaired work
and component-probe images, with complete live Rust BIN/Hunk equality. The prior
80-run matrix also covered ordinary and L1-only images. The repair only enlarges
and relocates opt-in diagnostic tables/report bands. Independent reviews found no
further concrete defect. Six focused Rust tests, Clippy with warnings denied,
Rust/native formatting, 48 Python tests and 147 workflow tests pass.
The focused redundant-test guard has zero blocking findings in changed assembly
files. Its broad repository run still flags 17 existing sites in unchanged files;
this checkpoint does not repair or claim that broad guard passes.

Final full-input receipts/summary:
`/tmp/opforge-template-inventory-full-v9-20261014/summary.json`.
Images: `/tmp/opforge-template-inventory-images-v9-20261014/images.json`;
native source identity `fnv1a64:f00fd63f6962945c`.
Final controls: `/tmp/opforge-template-inventory-controls-v9-20261014`.

The separate ordinary **full current-source self-host** completes in **102.877735
seconds**: 203 linked files / 2,625,156 source bytes, fresh START/DONE and zero exit,
with complete Rust and own-bootstrap equality for the **706,956-byte** Hunk
`fnv1a64:ba4df89ee2249d95`. It embeds only 68020 and enables no audit/memory flags;
the ordinary executable is unchanged from the preceding checkpoint. This qualifies
that default current-source build, separately from the frozen-input observer runs;
it is not an audit-enabled current-source self-host or A6000/2 MiB qualification.
Receipt: `/tmp/opforge-template-inventory-current-selfhost-20261014/manifest.json`;
source manifest `fnv1a64:fe2dd20e35f70198`.

### Next bounded candidate

The evidence supports a **generic first-use macro-body binding experiment**:
retain the packed definition recipe and its declaration context, defer body-plan
binding until the first accepted invocation, then reuse that result. Preserve
parameter/default semantics, lexical and conditional ownership, nested/include
behavior and existing error policies. Keep the eager path as the reference.
Do not select behavior by telemetry source names or benchmark identity.

Start with ordinary macros and explicit controls for unused, empty, repeated,
nested, shadowed and default-consuming calls. Carry declaration identities/context
without retaining borrowed views; unknown dependencies remain conservative. Count
actually avoided work and measure runtime and memory separately before expanding
to segment/statement templates. The ordinary-macro experiment above implements
this candidate; it does not extend deferral to those other template kinds.

## Measured reference and remaining uncertainty

The preceding work observer (`bbb2f648`) reconciles all 1,415 published whole units
with zero uncertainty/accounting errors on the frozen self-host workload: 198 files,
2,614,471 source bytes and 708,592 output bytes. Known unreferenced whole units
account for only 1,205 / 425,557 bindings (0.28%) and 52,628 ordinary capture bytes
(0.23%). This is a lower bound on explained unneeded work, not an upper bound on
all deferred-processing opportunity.

Captured-plan binding is concentrated in template-definition context: 18,823 /
20,731 calls (90.8%) and 2,821,828 / 3,082,134 committed bound-plan bytes (91.6%).
Those are work deltas, not uniquely owned retained allocations. Absence of an
invocation owner does not prove a definition is unneeded; the current inventory
investigates that distinction.

Reference tracked heap peak is 48,390,024 bytes; allocated/freed capacities are
113,993,896 bytes each. Capture uses 23,613,704 bytes in 33,255,424 reserved bytes.
This includes four-byte work-observer headers. The observer itself owns 689,440
fixed bytes before the new template inventory. All 164 regions become potentially
retireable by the end of replay, with no observed later-access contradiction;
last access alone does not establish safe consumer closure or peak savings.

Evidence: `/tmp/opforge-lazy-unknown-full-v6-20261014/summary.json` and
`/tmp/opforge-lazy-unknown-images-v6-20261014/images.json`. The preceding ordinary
current-source self-host separately completed with full Rust/own-bootstrap equality.
These measurements use the expanded-memory FS-UAE A4000/68040 profile: 2 MiB chip,
8 MiB fast, 64 MiB Zorro III and 16 MiB graphics. They do not qualify A6000 hardware
or the 2 MiB product target.

## L1 implementation and measured scope

`OPFORGE_LAZY_AUDIT` enables publication/lifetime observation; additionally setting
`OPFORGE_LAZY_WORK` enables whole-unit work attribution and the current template
inventory. Reporting requires the existing debug/memory/progress gates. Timing
builds omit those reporting gates; separate memory pairs enable identical gates.
Disabled macros emit no code, imports or references.

The [region observer](../../native/motorola68000/amigaos/debug/opforge_lazy_audit.asm)
records retained capture ranges, scheduled consumers, suspended include pins and
replay checkpoints. The [unit observer](../../native/motorola68000/amigaos/debug/opforge_lazy_units.asm)
reconciles authoritative block publication and selection, including preceding labels,
internal labels and ordered-copy offsets. The
[work observer](../../native/motorola68000/amigaos/debug/opforge_lazy_work.asm)
attributes work to those units and independently classifies definition/expansion/
ordinary call-time contexts. Tables store numeric identities and offsets.

Region cuts are allocation boundaries, not semantic unit boundaries. Whole-unit
attribution checks the global capture extent and all overlapping regions; close
before open, out-of-bounds and absent publication endpoints remain errors. Physical
and generated ranges must not double-own their caller's capture.

This remains publication-time evidence. Acquisition-only unit discovery and
individual binding/publication links are not implemented. Preparation with no
published block remains explicitly unassigned. Templates, constants, schema and
configuration need consumers separate from emitted block selection.

The existing legacy capture ambiguity for `.endmacro`/`.endsegment` parameter
operands remains a later opcore parity issue. A8 head classification covers
`.statement`/`.endstatement`; this audit does not replace the macro/segment grammar.

## What the current code establishes

| Boundary | Implemented behavior and implication |
|---|---|
| Acquisition | [frontend.captureLine](../../native/motorola68000/amigaos/experimental/binary_frontend.asm) tokenizes and captures candidate plans before whole-block selection. Deferring expression compilation during replay does not avoid this acquisition work. |
| Ordering | [configureCapturedModule/orderCapturedSources](../../native/motorola68000/amigaos/experimental/binary_source_schedule.i) revisit captures while discovering/configuring dependencies. File EOF is not their last use. |
| Replay | `replayCapturedSources` prepares graph-ordered spans, then releases all captures. [replaySourceSlice/materializePendingSource](../../native/motorola68000/amigaos/experimental/binary_source_cursor.i) can interrupt a parent cursor to acquire and replay an active include. |
| Binding completion | `frontend.complete` calls `scopes.finish`, then refreshes NameCount: final member acquisition can add identities. [imports.finish](../../native/motorola68000/amigaos/experimental/binary_imports.asm) can grow the entry arena and rewrite member/proxy targets. |
| Selection | [app.prepare](../../native/motorola68000/amigaos/experimental/binary_app.asm) completes binding and materialization before block indexing/selection. Unused block omission currently saves execution, not all preparation. |
| Frozen session | [assembly.execute](../../native/motorola68000/amigaos/experimental/binary_assembly.asm) pins record base/extent, context, symbol count/cells, member keys and module boundaries. `advance` requires the complete frozen frontier. [repeat_inventory.refresh](../../native/motorola68000/amigaos/experimental/binary_repeat_inventory.asm) rejects a changed symbol count. |

Existing [measurements](native-fragment-preparation.md#first-experiment-results)
show a 47,587,208-byte tracked heap peak on the later full input. The fragment
wrapper adds 2,122,664 transient allocated bytes and avoids no preparation.
The earlier [header comparison](native-performance-diagnosis.md#compact-retained-capture-headers)
has 31,862,784 bytes of retained capture capacity and substantial binding time.
These are different inputs/configurations, not a combined before/after pair.
They motivate this work but do not quantify how much preparation is unnecessary.

## Inventory: identity before activity

Keep four identities distinct:

| Identity | Meaning and lifetime |
|---|---|
| Source occurrence | Acquisition owner ID plus record offset, with physical origin. Stable through owner movement. Generated occurrences also identify the parent occurrence and invocation/iteration path. |
| Structural unit | Owning module/scope and complete block, template or control range. Includes the preceding entry label where applicable; internal labels remain part of the same block. |
| Binding occurrence | A use/declaration in a lexical and configuration context. It can refer to a potential owner without claiming that owner is active or its value exists. |
| Published symbol/site | The authoritative numeric result of shared binding and expansion. One source occurrence can produce several results; synthetic member identities need not have a standalone declaration line. |

Do not reserve every final runtime symbol ID from physical lines. Templates,
iterations, embedded substitutions and final member acquisition make that promise
false. Existing numeric IDs remain authoritative within the sealed session;
object-local remapping belongs to the later persistent-object work.

The structural inventory records bounded ranges, enclosing owners, declaration
candidates, control nesting, template boundaries, origin and configuration context.
It reads binary captures, never original source text. Reuse shared statement/loop/
file classifiers and scope services: `source_control.scanBody/classifyLoop/fileAction`,
`scopes.statementRole`, and frontend role callbacks. Extend a shared classifier
when information is missing; do not recognize directives with another native parser.
Lexical tokens can retain normalized spelling until binding; published execution
continues using numeric identities.

Candidate references inside an unresolved alternative are **potential dependencies**,
not live roots. A reserved identity does not grant executed visibility, value
availability, immutable certification or reachability. These observations stay
separate, preserving longest-owner lookup and conditional ownership.

Whole referenced `.block` units retain all internal/fallthrough code. Constants,
schemas, configuration values and template definitions can be required without
emitting code. Macros/segments are expansion templates, not block-pruning units.
Proven inactive semantic alternatives trigger no file acquisition, package
activation or live block roots. Inspecting their already captured structure does
not establish activity; unresolved alternatives retain the existing pending/eager
policy. Module dependency discovery is distinct from executing those alternatives.
Discovery must preserve existing structural/error policies;
deferred preparation must not silently accept previously rejected input.

## Publication: proposed pre-session contract

The first lazy candidate operates entirely **before** `beginSizing`. It builds one
complete selected prepared stream and uses the existing frozen assembly driver.
It does not relax a session check, append records during sizing, or use an invented
preparation PC to evaluate assembly-dependent guards.

These are proposed operations, not existing callable APIs:

| Operation | Required contract |
|---|---|
| Inventory append | Add complete occurrence/unit rows after validating bounds and nesting. IDs are append-only during acquisition; no partial row is visible after failure. |
| Request unit | Record the requesting context and reason: output root, block reference, value/schema need or template expansion. Do not execute it just because it was discovered. |
| Prepare closure | Use existing bind/EXVM/PRVM/template owners in dependency and source-effect order. Prepare whole requested blocks and needed helpers; unknown dependencies remain pending. |
| Publish batch | Stage complete records, binding results, typed owners, origins and section/context mappings. Publish their consistent extents together only after validation. Never expose half a record or a pointer into retired staging storage. |
| Seal | Complete imports/member plans, reconcile symbol count and owner inventory, export cells/typed values/keys, and materialize the final stream. Rebuild offset indexes against that stream before `beginSizing`. |
| Retire | Release an acquisition owner only after consumer closure; release staging owners after commit or abort. Preserve the previously published state on failed staging. |

Request order must not become language execution order. Mutable assignments,
configuration changes and other effects retain their graph-ordered/source-ordered
meaning. Shared evaluators remain authoritative. Preparation-known immutable
dependencies may be requested early without publishing them as executed values.

When activity depends on placement, instruction width, a mutable value or an
unavailable forward value, the first lazy candidate prepares that enclosing unit
through the reference path. This is conservative **extra preparation**, not
permission to select an inactive branch or retain extra output. Record the fallback
reason. Final reachability, guard evaluation and convergence remain unchanged.

Actual-state `.include` acquisition during sizing requires the separate
[growing source-session work](native-source-execution.md#assembly-session-and-stable-publication):
appendable declaration enrollment, stable consumed references, transactional
cell/view growth and replay observations. Allocation relocation alone does not
provide those semantics. Current supported preparation-time active/expanded
includes remain covered; the new scheduling must not introduce eager file loads.

## Lifetime: release owners, not convenient source boundaries

[capture.create/resolve/planView](../../native/motorola68000/amigaos/experimental/binary_capture.asm)
copy inputs into an owned record and return borrowed bounded views. The temporary
candidate arena is already released after construction; the reusable CaptureLine
is copied into a retained region. A region can contain several semantic spans or
only part of a large file, so neither file EOF nor block end establishes retirement.

| Owner | Consumers and release condition |
|---|---|
| Capture region | Configuration scans, scheduled replay spans, physical peek/seek, suspended parent cursors and pending body/template work. Retire only after all relevant ranges are consumed or copied to independent owners. |
| Region directory | Logical range lookup and saved region indices. Retain slot identity while consumers exist. A retired slot must explicitly reject resolution; a null arena with old Base/End is insufficient. |
| Template/plan storage | `macro_plans.bind` creates a session-owned plan; `templates.storeBody` copies records into its BODY pool. Verify retained recipes and views are independent before counting this as transfer of ownership. |
| Prepared records | Own numeric records/capsules through sizing, emission and output provenance. The frozen session borrows this allocation; it cannot move or grow until session release. |
| Binding/typed metadata | Preparation owns scope/proxy work; export copies required plans, parameters, seeds and member keys before frontend retirement. Inactive metadata must not publish inactive symbols. |
| Origin paths/output provenance | Keep the diagnostic/listing information required by prepared records independently of capture payloads. Release after their final output/diagnostic consumers. |

Use owner ID and offsets in retained structures. Native allocation descriptors
may hold process pointers; cached views are reconstructed for bounded calls and
must not survive owner movement or retirement.

At first, use last-consumer positions for graph-ordered spans plus explicit pins
for suspended include cursors and retained/deferred ranges. Avoid a per-token
reference-counting system. Newly acquired include regions join the same accounting.
If a region mixes live and consumed ranges, retain it; copying retained subranges
into smaller owners is a later, separately measured option.

On failure, discard unpublished staged owners and keep prior published state
intact. Cleanup releases remaining live owners exactly once, including partially
acquired includes. Diagnostic origin survives long enough to report the failure.

## Choosing the optimization after L1

- **L2 lifetime candidate:** if closure is established, release consumed regions
  after their last replay consumer. This may reduce overlap/late live storage;
  acquisition peak can remain unchanged. Keep tombstones and failure cleanup valid.
- **L2 deferred-work candidate:** if inventory is sufficient and unused-body work
  is substantial, defer one class of plan/binding/expression preparation for complete
  unrequested blocks. Keep uncertain units eager and count the actual avoided calls
  and bytes. Avoiding replay compilation alone does not avoid captured plans.

Choose one candidate first from observed opportunity, not both in one comparison.
Preserve the working reference. Compare identical source/package/configuration
inputs with release timings and separate ownership measurements, then perform a
fresh full current-source self-host. Report each change's time and memory impact
independently. Package demand loading and persistent `.ofm` remain later stages.

## Validation and reproduction

The maintained controls compare reference, L1-only, work and component-probe images
against live Rust BIN/Hunk oracles. They cover guard outcomes, whole blocks,
preceding/internal labels, imports/forward members, generated names, loop shadowing,
schemas/configuration, empty/default/nested templates and include suspension/resumption.
Component probes check register/CCR/stack preservation, reset, overflow/capacity,
conservation, owner overlap and cross-region bounds. Any incomplete attribution
remains visible; it never changes eager execution.

The [comparison runner](../../scripts/performance/run_lazy_preparation_comparison.py)
checks file/image digests, actual defines, fresh completion and complete Rust output.
Its [decoder](../../scripts/performance/lazy_preparation_audit.py) reports inventory
coverage independently of native completion and memory measurements. A frozen-input
comparison is not a current-source self-host qualification; perform that separately.

```sh
OPFORGE_LAZY_SAVE_DIR=/tmp/new-images OPFORGE_LAZY_SAVE_PROBE=1 \
  cargo test -p asm native_lazy_audit_save_images -- --ignored --nocapture --test-threads=1

python3 scripts/performance/run_lazy_preparation_comparison.py \
  --work-attribution --baseline-images /absolute/previous-work-images \
  --workload-case /absolute/frozen-full-case/case.json \
  --images /tmp/new-images --output /tmp/new-results \
  --test-binary /absolute/asm-tests

OPFORGE_DEFERRED_SAVE_DIR=/tmp/new-first-use-images OPFORGE_DEFERRED_SAVE_PROBE=1 \
  cargo test -p asm native_deferred_macro_save_images -- --ignored --nocapture --test-threads=1

python3 scripts/performance/run_lazy_preparation_comparison.py \
  --experiment-define OPFORGE_DEFERRED_MACRO_PLANS \
  --workload-case /absolute/frozen-full-case/case.json \
  --images /tmp/new-first-use-images --output /tmp/new-first-use-results \
  --test-binary /absolute/asm-tests
```

For a separate current-source experimental self-host, set
`OPFORGE_COMPACT_EXPORT_DEFERRED_MACRO_PLANS=1`,
`OPFORGE_COMPACT_EXPORT_OUTPUT_EMBED=68020`, `OPFORGE_COMPACT_EXPORT_NATIVE=1`
and a fresh absolute `OPFORGE_COMPACT_EXPORT_DIR`, then run the ignored
`export_compact_self_host_bundle` test. Its oracle, bootstrap, relocated-source
check and guest command all use the same configured source. The exporter copies
the native source into a temporary build root and generates
`experimental/build_configuration.i`, defining `DEFER_MACRO_PLANS = 1` in
the three owning modules. The repository include maps the Rust preprocessor
switch to that constant, selecting zero by default. The
generated origin and configuration are explicit in the manifest; no native `-D`
option is needed or added. Hardware qualification remains a separate case.

Use the maintained FS-UAE environment and preserve evidence/tools outside `target`.
Do not overlap heavy host work with reported runtime samples. Compare release
runtime and separate tracked-heap/linked-reservation pairs on identical inputs.
A6000 is unavailable for this checkpoint.
