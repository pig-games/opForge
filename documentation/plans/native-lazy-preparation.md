# Lazy preparation: inventory, publication and lifetime

Status: design and source audit, 2026-10-14. No lazy execution or early retirement
is implemented by this document. The [fragment experiment](native-fragment-preparation.md)
is qualified; the ordinary path remains the reference.

## Decision and first checkpoint

Separate structural discovery, semantic publication and allocation ownership.
Start with an opt-in **shadow inventory and lifetime audit** beside unchanged
preparation. Reconcile its observations with the existing completed binding and
block inventory. Do not skip preparation or release captures in this checkpoint.
This is a bounded implementation experiment, not a requirement to design the
whole `.ofm` format first.

The next optimization is selected from its results: defer expensive body work
where the structural inventory is sufficient, or retire capture regions whose
last consumer is proven. Measure those changes separately. Early retirement
alone cannot eliminate the peak already reached while acquiring all files.

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

## L1: shadow inventory and lifetime experiment

Hypothesis: a compact structural inventory can explain authoritative whole-unit
ownership and identify capture last consumers without executing bodies or duplicating
the expensive semantic preparation. L1 tests this; it does not claim savings.

Implement the observer outside the large app/frontend. Their hooks report shared
classification, authoritative binding/publication events and replay boundaries.
Keep tables per structural unit/region, not another token/line copy. Use reusable
gated telemetry macros; the release reference has no observer overhead. Any bounded
table exhaustion is an explicit incomplete audit, never a successful verdict or a
reason to change production semantics.

1. Record occurrence/unit ranges during unchanged acquisition. Track generated
   occurrence relationships during expansion; mark unavailable classification
   explicitly instead of guessing final names or activity.
2. Reconcile binding/member results after `frontend.complete`; reconcile block
   ranges and preceding/internal labels after ordered materialization and block
   indexing; compare reachability after selection. Preserve an explicit mapping
   from acquisition occurrences through original prepared offsets to materialized
   offsets. Use authoritative binding events and the existing completed indexes.
   Require complete relationships for the bounded controls/supported categories;
   generated or synthetic relationships without event links remain deferred. Do
   not duplicate the whole symbol/value/schema system to make the report complete.
3. Track capture consumers and independently compute retirement eligibility from
   scheduled spans, saved include cursors and verified transfer into independent
   template/plan/provenance owners. Also flag any later lookup after a predicted
   retirement point. A last-access trace only describes the exercised traversal;
   it cannot alone establish consumer closure. Unexplained relationships keep the
   affected regions live. Keep the real all-captures release unchanged.
4. Report inventory cost, deferred relationships, reasons for retention, capture
   used/capacity by replay boundary, and the capacity that could be retired.
   Account for allocation-growth overlap. Report a hypothetical capture-residency
   curve separately from actual MEMD peak; it is not measured memory savings.

Focused controls cover nested/preceding/internal block labels, transitive imported
references, a forward imported member, both guard outcomes, loop shadowing, generated
macro names, typed schema/configuration needs, and an include that suspends/resumes
its parent. Force region rollover and allocation failure. Compare fresh native
outputs with the unchanged path and live Rust; compare bindings/ownership at the
actual publication boundary, not before member completion.

Focused fresh parity and bounded observer invariants form the first checkpoint.
Once the observer is stable, audit the same frozen full self-host input used by
the reference for coverage and cost. Full-input unresolved relationships are
reported, not treated as retireable. Require fresh completion, explicit zero exit
and entire Rust Hunk equality for a completed native full-input audit; this
qualifies only that configuration. Measure observer runtime, allocated/peak bytes
and static cost separately. Do not call an instrumented-versus-release difference
an optimization result. A6000 is unavailable; identify the FS-UAE profile explicitly.

Success is explained ownership for the bounded controls and no unaccounted consumer
in a proposed retirement interval. Report remaining relationships conservatively.
Stop/reconsider if the observer needs a second grammar/evaluator, a complete duplicate
semantic inventory, unbounded tables or weakened frozen-session guarantees.

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
