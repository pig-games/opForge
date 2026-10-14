# Lazy preparation: inventory, publication and lifetime

Status: opt-in publication/lifetime, whole-unit work and template-definition/consumer
inventories are measured and natively qualified, 2026-10-14.
The ordinary eager path remains the reference. No preparation is skipped and no
capture is retired early. Historical checkpoint details are in Git history.

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
to segment/statement templates. This is a recommendation, **not implemented here**.

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
```

Use the maintained FS-UAE environment and preserve evidence/tools outside `target`.
Do not overlap heavy host work with reported runtime samples. Compare release
runtime and separate tracked-heap/linked-reservation pairs on identical inputs.
A6000 is unavailable for this checkpoint.
