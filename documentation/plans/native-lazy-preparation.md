# Lazy preparation: inventory, publication and lifetime

Status: opt-in L1 publication/lifetime and whole-unit work audits implemented,
measured and natively qualified, 2026-10-14.
The ordinary path remains the reference. No preparation is skipped and no capture
is retired early. Acquisition-only unit discovery and individual binding links
remain unimplemented; see the measured scope below.

## Active investigation: remaining unknown work

The observer now separates costs on published units with uncertainty reasons from
the arithmetic global/unassigned remainder. These are independent of a new
call-time classification: template definition, expansion or ordinary processing,
each with or without an active invocation owner. This classification uses existing
template state. It does not identify inactive branches, unused templates or work
that a lazy executor could safely omit.

The optional report carries three largest reason-marked contributors, four bounded
range exemplars and their owned source paths. Each vector conserves the existing
18 global totals; context totals independently conserve captured-plan calls and
committed bound-plan bytes. Source paths are read before their owner is released.
No pointers survive reporting, no new heap allocation is introduced, and hooks
preserve registers, CCR and stack. Fixed observer reservation grows by 684 bytes;
the app path adapter uses 12 bytes of temporary stack storage.

### Proven observer defect: region cuts are not unit boundaries

The diagnostic run found two whole units spanning retained capture regions. For
example, `selectLine .block` in `src/experimental/binary_source_selection.i`
opens at capture offset 1,249,532 and closes at 1,269,768, beyond its first region's
end at 1,262,564. The previous observer incorrectly marked this `BAD_RANGE`.

The observer now checks the actual global capture bound and attributes matching
records across every overlapping region. Generated boundaries still do not own
their caller's physical capture. Close-before-open, out-of-global-bound and missing
publication endpoints remain invalid. Region cuts and continuations are reported
as observations, not automatically as errors. A native component probe owns exactly
two records across two independent regions, including a close equal to the first
region's end, without double counting or demotion.

The eager reference remains unchanged. Ordinary and L1-only timing/memory images
are byte-identical to the preceding checkpoint. No binding, compilation or capture
work is skipped; no payload is released early.

### Separate identical-input runtime comparison

The frozen input remains 198 files / 2,614,471 source bytes, with a complete
708,592-byte live Rust oracle. Previous and revised work observers use the same
audit flags and embedded packages. Timing builds have no memory telemetry.

| Serial timing pair | Previous observer (s) | Revised observer (s) |
|---|---:|---:|
| Previous then revised | 110.702892 | 109.641136 |
| Revised then previous | 109.123843 | 110.592792 |
| Mean | 109.913367 | 110.116964 |

The incremental observer difference is **+0.203597 s / +0.19%**, within the
run-to-run spread. All four runs complete with fresh START/DONE, zero exit and
complete Rust equality. This measures diagnostic overhead, not deferred-work
savings. The existing expanded-memory A4000/68040 profile remains in use; these
numbers do not qualify A6000 hardware or the 2 MiB product target.

### What this investigation can establish

The separate memory pair has the same tracked heap peak of **48,390,024 bytes**
and the same **113,993,896 bytes** of allocated/freed capacity. Terminal live heap,
allocation failures and profiling errors are zero. Linked reservation grows
**1,812,632 → 1,814,960 bytes (+2,328)**, including code and fixed observer state.
Capture used/capacity remain **23,613,704 / 33,255,424 bytes**. All six full-input
runs complete with full live Rust equality; every one of the 18 global work totals
is unchanged.

After the range repair all 1,415 published units reconcile with zero uncertainty
reasons and zero accounting errors. Unknown capture falls **47.33% → 47.13%**;
48,580 bytes now belong to selected units. The repair also assigns 1,153 bindings,
223 EXVM compilations and 304 process calls to selected units. No new work becomes
unreferenced. Known unreferenced work remains only 1,205 / 425,557 bindings (0.28%)
and 52,628 ordinary capture bytes (0.23%, excluding observer headers).

| Call-time context | Captured-plan bindings | Committed bound-plan bytes |
|---|---:|---:|
| Template definition, no invocation owner | 18,823 | 2,821,828 |
| Expansion, no invocation owner | 0 | 1,048 |
| Ordinary, no invocation owner | 1,372 | 173,384 |
| Template definition, with invocation owner | 0 | 0 |
| Expansion, with invocation owner | 0 | 11,416 |
| Ordinary, with invocation owner | 536 | 74,458 |
| Total | 20,731 | 3,082,134 |

Template-definition preparation without an active block owner accounts for
**90.8% of captured-plan binding calls and 91.6% of bound-plan bytes**. The remaining
unknown vector is entirely the global/unassigned remainder, not reason-marked
published units. These contexts precede final preceding-label transfer, so they
are independent of final ownership categories. Bound bytes also include plan
creation that does not call `bindCapturedPlan`; zero calls need not mean zero bytes.

This points to template preparation as the next investigation, but does not prove
those definitions are unreferenced or quantify avoidable elapsed time/heap peak.
The smallest next step is a template consumer inventory: identify definitions,
active expansions, shared/configuration effects and retained plan/capture payload
per definition. Then choose a bounded deferred-body-binding experiment. Do not
infer safe deferral from the absence of a block owner. Preserve binary templates,
complete referenced blocks and the eager correctness reference.

Receipts: `/tmp/opforge-lazy-unknown-full-v6-20261014/summary.json`;
72 fresh focused controls: `/tmp/opforge-lazy-unknown-controls-v6-20261014`;
final images: `/tmp/opforge-lazy-unknown-images-v6-20261014/images.json`.
Native source digest: `fnv1a64:963d1e43773229a0`.
The probes cover exact cross-region ownership and global-bound rejection, context
conservation, zero-absent exemplars and register/CCR/stack preservation. Focused
Rust checks, Clippy with warnings denied, Rust/native formatting, 22 Python tests
and the workflow checks with 147 tests pass. Independent review found no further
grounded defects. The redundant-test guard has zero blocking findings; existing
call-return checks remain advisory.

The separate ordinary **full current-source self-host** also completes in
**105.076963 seconds** on FS-UAE: all 201 files / 2,614,816 source bytes, fresh
START/DONE and zero exit. It reproduces both the complete live Rust Hunk and its
own **706,956-byte** bootstrap, `fnv1a64:ba4df89ee2249d95`, with only the embedded
68020 package and no defines. Source manifest: `fnv1a64:e84b805888c10f97`.
Receipt: `/tmp/opforge-lazy-unknown-current-selfhost-20261014/manifest.json`.
This is current-source correctness proof, not another comparative timing sample.

## Previous checkpoint: invocation and non-emitting ownership

The work observer now snapshots the deepest authoritative physical unit at replay
begin, after the capture origin is known. Preparation follows that invocation
owner, while prepared-copy bytes follow the actual publication owner. Physical
block openings establish their new owner. Publication consumes a sample without
ending the invocation; replay completion, failure and include suspension flush
remaining work and clear the owner, including calls that emit no prepared record.
This prevents subsequent global work from inheriting a stale unit.

Generated publications retain their own uncertainty without demoting their
physical caller merely because expansion is active. Generated boundaries do not
claim the caller's retained physical capture. Records with a different origin do
not poison an otherwise valid physical range; actual cross-file execution,
including zero-output execution, still marks the affected unit `MULTIFILE`.
Ambiguous generated preceding-label transfers remain `PREVIOUS_UNKNOWN`: the
existing containment check must prove the source vector before moving it.

This adds one four-byte observer scalar, bringing work-table reservation to
**688,756 bytes**. There is no new journal, capture/header/row growth, parser,
evaluator, deferred execution or early release. Ordinary release and L1-only
timing/memory images are byte-identical to the preceding checkpoint. The eager
reference remains unchanged.

### Focused ownership proof

Seventy-two fresh native runs cover 18 controls on ordinary, L1, refined-work and
work-probe images. All complete with zero exit and full live Rust BIN/Hunk equality.
Paired selected/unreferenced controls have identical complete preparation vectors;
only their final category differs. Their physical caller has no uncertainty reason.

| Macro in the same `costed` block | Captured-plan bindings | Bound-plan bytes | Process calls |
|---|---:|---:|---:|
| Empty conditional expansion | 1 | 44 | 8 |
| One emitting expansion | 1 | 80 | 6 |
| Nested emitting expansion | 1 | 160 | 8 |

The empty control deliberately has no active emission in its macro; its counts
detect the publication-only observer's lost work. Probes independently check exact
zero-output counts, duplicate flush, cleared owners, zero-output origin mismatch
and register/CCR/stack preservation. Existing generated-unit, include, preceding-
label and ordinary-body controls remain in the matrix. Template retention stays
shared, and every metric conserves its global total.

### Identical full input: attribution and overhead

Six fresh full-input runs use the same frozen 198 files / 2,614,471 source bytes
and match the complete 708,592-byte live Rust Hunk. The previous and refined
observers use identical audit flags and packages. All 18 global work totals are
exactly unchanged. All 1,415 units reconcile, with zero observer/accounting errors.
The existing expanded-memory A4000/68040 profile remains in use; this is neither
A6000 nor 2 MiB qualification.

| Serial timing pair without memory telemetry | Previous observer (s) | Refined observer (s) |
|---|---:|---:|
| Previous then refined | 108.550765 | 108.222396 |
| Refined then previous | 106.588735 | 106.679345 |
| Mean | 107.569750 | 107.450870 |

The difference is **−0.118880 s / −0.11%**, within run-to-run variation. There is
no measured preparation speedup: neither image skips work. In the separate memory
pair, tracked heap peak is **48,390,024 bytes** for both; allocated and freed
capacities are **113,993,896 bytes** each, with zero terminal live heap, allocation
failures or profiling errors. Linked reservation changes **1,812,256 → 1,812,632
bytes (+376)**. Capture used/capacity are unchanged at 23,613,704 / 33,255,424 bytes.

| Resource still unknown | Previous | Refined | Newly attributed to required units |
|---|---:|---:|---:|
| Binding calls | 199,352 | 120,597 | 78,755 |
| EXVM compilations | 31,625 | 16,973 | 14,652 |
| Process calls | 65,961 | 38,858 | 27,103 |
| Capture bytes, including audit headers | 14,465,068 | 11,177,508 | 3,287,560 |
| Captured-plan bindings | 20,731 | 20,195 | 536 |
| Retained bound-plan bytes | 3,082,134 | 2,996,260 | 85,874 |

Unknown capture ownership falls **61.26% → 47.33%**. The known unreferenced
fraction remains **1,205 / 425,557 bindings (0.28%)** and **52,628 / 23,155,688
ordinary capture bytes (0.23%, excluding observer headers)**. No newly explained
cost becomes unreferenced on this input. Shared template payload remains 640,694
bytes; shared means unapportioned, not proof that every definition is needed.

This strengthens the evidence that a large amount of measured preparation belongs
to required code. It still cannot bound all savings from deferral. Work outside
published units, unpublished/inactive units and physical ranges marked `BAD_RANGE`
remain unresolved; the final reason mask is 16. Most captured-plan binding and
bound-plan payload are still unknown. Acquisition-only inventory and distinguishing
global residual work from ambiguous unit work are prerequisites to a broader claim.
Do not equate call/payload fractions with elapsed-time or heap-peak savings.

Receipts and captures: `/tmp/opforge-lazy-owner-full-v3-20261014/summary.json`;
focused matrix: `/tmp/opforge-lazy-owner-controls-v3-20261014`; images and disabled
equivalence proof: `/tmp/opforge-lazy-owner-images-v3-20261014`. Native source
digest: `fnv1a64:54febc6625bfb044`.

The separate ordinary **current-source self-host** also completes in **103.844493
seconds**: 199 files, 2,610,057 source bytes, fresh START/DONE and zero exit. Native
reproduces the entire live Rust Hunk and its own 706,956-byte bootstrap, both
`fnv1a64:ba4df89ee2249d95`, with only the embedded 68020 package and no defines.
Source manifest: `fnv1a64:1e442dc9740d6d35`. Its qualified export is at
`/tmp/opforge-lazy-owner-current-selfhost-20261014/manifest.json`. This single run
is a self-host correctness checkpoint, not a separate performance comparison.

Focused Rust tests, Clippy with warnings denied, Rust/native formatting, the
redundant-test guard, 14 Python audit/comparison tests and the workflow checks with
147 tests pass. Validation is scoped to the observer and its unchanged eager path.

Reproduce the refinement comparison using the maintained FS-UAE environment:

```sh
python3 scripts/performance/run_lazy_preparation_comparison.py \
  --workload-case /absolute/frozen-full-case/case.json \
  --images /absolute/refined-images --baseline-images /absolute/previous-images \
  --work-attribution --output /absolute/new-results --test-binary /absolute/asm-tests
```

## Baseline checkpoint: publication-only preparation attribution

`OPFORGE_LAZY_WORK` is implemented beside `OPFORGE_LAZY_AUDIT`, observing the existing eager
path. The hypothesis is that complete unreferenced blocks account for a measurable
fraction of preparation calls and retained capture/plan bytes. This checkpoint
measures that fraction; it skips no work and changes no release representation.

Count actual candidate, descriptor, fragment and plan-creation calls during physical
capture, including unsuccessful attempts. A four-byte audit-only capture field
carries those counts until structural ownership is available. Attribute replay
binding/expression/plan/process calls and retained used bytes through publication
provenance, then reconcile against authoritative whole-block selection. Charge
nested work exclusively to its deepest proven owner; include the preceding entry
label only where the final block extent owns it.

Partition each resource total into selected, unreferenced, shared/required and
unknown ownership. Shared templates, schemas/configuration effects, generated
occurrences, interleaved includes and ambiguous publication must not inflate an
unreferenced-block opportunity. Keep capacity/slack distinct from payload used;
never prorate shared capacity to claim a peak-memory saving. Bounded observer
exhaustion, counter overflow, incomplete ownership and truncated printed detail
are separate report states.

Qualify small selected/unselected and nested controls against the unchanged path,
check aggregate conservation and instrumented preservation, and compare the
same frozen full input. Measure marginal runtime overhead with L1-only versus
L1+work images without memory telemetry, then memory in a separate identically
configured pair. A fresh current-source default self-host remains a separate
proof. Ordinary releases must contain no observer code, storage or extra capture
fields. Stop attribution claims on accounting failures; unknown ownership remains
visible. The unreferenced fraction is an upper bound on a later candidate, not
evidence that deferral preserves all diagnostics or saves the same fraction of
elapsed time.

### Attribution scope and interpretation

The [work observer](../../native/motorola68000/amigaos/debug/opforge_lazy_work.asm)
owns separate capture, partition and report includes; it reuses L1's published
unit intervals rather than adding a source parser. Its 4,096 bounded unit rows
and regional scratch filter reserve **688,752 linked bytes**, in addition to L1.
All of that storage and its calls disappear without both audit flags. The capture
header is 56 bytes normally and 60 bytes with both flags. The report names the
four-byte-per-record instrumentation cost separately from retained payload.

The 18 metrics distinguish physical candidate/descriptor/fragment/plan-creation
calls, replay binding/EXVM/captured-plan/process calls, retained plan and template
pool growth, preparation/replay copies, and capture header/token/lexeme/plan/padding
bytes. Capture bytes are observed before their normal release. Plan bytes measure
successful retained `MACRO_PLANS` appends; template bytes cover definition rows,
bodies and default/spelling payload pools. They are allocation-stage observations,
not a simultaneous live-memory sum. Symbol tables, schema/member indexes, package
directories and arena slack are not apportioned as unit payload. Memory telemetry
measures the process's tracked heap separately.

Selected ancestors retain their entire nested bodies. Resource counts go to the
deepest proven owner, and a preceding entry label transfers only when the final
block extent includes it. Shared template publication remains shared; declarations
with possible semantic effects, generated/multiple owners, cross-file activity,
nonmonotonic activity within a unit and unresolved publication remain unknown.
Independent graph reordering of disjoint units is permitted. The four categories
sum exactly to the global total for every metric; printed unit rows can sum to less
because global/shared work has no block row. Detail printing stops at 16, while
aggregate accounting covers all bounded rows. Complete accounting does not mean
complete ownership coverage.

Forty-eight final focused fresh native runs compare reference, L1, attribution and
attribution-probe images on 12 controls, with complete live Rust BIN/Hunk equality.
The identical `costed` body has 18 bindings, four EXVM compilations, eight process
calls, 602 replay-copy bytes and 1,540 retained capture bytes whether selected or
unreferenced. Of those capture bytes, 32 are observer headers; the ordinary payload
is 1,508 bytes. It has 18 candidate, 16 descriptor, seven fragment and five
plan-creation calls. Changing only its external reference changes its category,
not its preparation cost. Probes check register/CCR/stack preservation, exact
register-operand counts, overflow/reset, ancestor selection and invalid captures.
This demonstrates the intended attribution mechanism; it is not a timing gain.

### Full-input attribution and observer overhead

Six serial fresh native comparisons use the same frozen input as L1: 198 files,
2,614,471 bytes, complete 708,592-byte live Rust Hunk equality. The current
bootstrap source digest is `fnv1a64:6df1820e8efb08b9`; both bootstraps embed 68020
and 6502 packages, while the frozen output embeds 68020. These comparisons retain
the existing A4000/68040 expanded-memory profile described below. They are fixed-input
measurements, not current-source self-host or hardware/2 MiB qualification.

| Serial timing pair | L1 only (s) | L1 + work (s) |
|---|---:|---:|
| Reference then work | 101.943206 | 106.686020 |
| Work then reference | 102.350905 | 107.125591 |
| Mean | 102.147055 | 106.905806 |

Marginal observer cost is **4.758750 seconds / 4.66%**. These images have no
memory telemetry. No work is avoided. Ordinary release and L1-only images are
byte-identical to the preceding checkpoint in both timing and memory modes.

| Separate memory pair | L1 only | L1 + work | Increase |
|---|---:|---:|---:|
| Tracked heap peak | 47,587,208 | 48,390,024 | 802,816 |
| Total allocated/freed capacities, each | 112,389,800 | 113,993,896 | 1,604,096 |
| Linked reservation | 1,120,196 | 1,812,256 | 692,060 |
| Used capture bytes | 23,155,688 | 23,613,704 | 458,016 |
| Capture capacity | 32,452,608 | 33,255,424 | 802,816 |

The used-byte difference is exactly 114,504 four-byte observer headers. The
capacity increase explains the tracked-heap increase. Linked plus tracked peak
increases 1,494,876 bytes; OS allocations, allocator metadata and stack remain
outside this accounting. Both runs release all tracked capacity and have zero
profiling diagnostics. The additional observer is not a release-memory regression.

All 1,415 publication units reconcile with the ordinary block index; work errors
and accounting failures are zero. Ownership coverage remains partial. Known
unreferenced whole units account for:

| Resource | Total observed | Known unreferenced | Fraction |
|---|---:|---:|---:|
| Binding calls | 425,557 | 1,205 | 0.28% |
| EXVM compilations | 69,365 | 154 | 0.22% |
| Candidate calls | 251,590 | 672 | 0.27% |
| Record processing calls | 121,885 | 336 | 0.28% |
| Replay-copy bytes | 11,703,923 | 30,617 | 0.26% |
| Ordinary capture payload, excluding audit headers | 23,155,688 | 52,628 | 0.23% |

Unknown ownership accounts for 199,352 bindings, 31,625 EXVM compilations and
14,465,068 instrumented capture bytes (61.26%). All 20,731 captured-plan binding
attempts and 3,082,134 retained plan bytes are unknown. Template payload of 640,694
bytes is shared. The report's reason mask is 17: generated activity and ambiguous
physical capture ranges. It deliberately demotes whole affected units rather than
guessing a division between physical and generated work. Printed rows are only
the first 16; the table above uses the complete aggregate partition.
Global work outside published block records also stays unknown. In particular,
preparation that emits no record has no publication sample to attach to a unit;
the row reason mask does not explain every global residual byte or call.

These results establish a small known unreferenced opportunity on this self-host
input, **not** a small upper bound on all deferred processing. The large unknown
fraction prevents that conclusion. Before implementing broad deferral, investigate
invocation/caller provenance, preparation with no emitted record and physical-range
attribution so ordinary units that contain generated work can be classified without losing their whole cost. Required
work must still be performed once; delaying it alone does not save CPU time. A
separate lifetime candidate remains possible but must measure its actual peak.

Receipts, preserved captures and summary:
`/tmp/opforge-lazy-work-full-v6-20261014/summary.json`. Current images:
`/tmp/opforge-lazy-work-images-v6-20261014/images.json`. Focused controls/probes:
`/tmp/opforge-lazy-work-controls-v6-20261014`.

The separate ordinary **current-source self-host** also completes: 199 files,
2,609,635 source bytes, a 706,956-byte Hunk, fresh native exit zero and complete
equality with its own bootstrap and live Rust in 102.612610 seconds. Bootstrap
and output both embed only 68020; no audit or memory flags are enabled. Its source
manifest digest is `fnv1a64:4aa9b3d0c158eff4` and output digest
`fnv1a64:ba4df89ee2249d95`. The qualified bundle manifest and proof are preserved
at `/tmp/opforge-lazy-work-current-selfhost-20261014/manifest.json` and the matching
log `/tmp/opforge-lazy-work-current-selfhost.log`. This proves that exact default
self-build configuration, separately from the frozen-input audit comparisons.

```sh
OPFORGE_LAZY_SAVE_DIR=/tmp/new-work-images OPFORGE_LAZY_SAVE_PROBE=1 \
  cargo test -p asm native_lazy_audit_save_images -- --ignored --nocapture --test-threads=1

python3 scripts/performance/run_lazy_preparation_comparison.py \
  --work-attribution --workload-case /absolute/frozen-full-case/case.json \
  --images /tmp/new-work-images --output /tmp/new-work-results \
  --test-binary /absolute/asm-tests
```

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

## L1 implementation and measured scope

`OPFORGE_LAZY_AUDIT` alone enables the audit work. Reporting additionally requires
the existing debug/memory/progress gates. Timing builds omit those gates; the
memory pair enables the same four gates on both images. Disabled audit macros
emit no code, imports or references. The default reference release executable
was byte-identical to the pre-L1 reference before the shared discovery repair
below. The final reference and audit images both include that repair.

The [region observer](../../native/motorola68000/amigaos/debug/opforge_lazy_audit.asm)
records capture extents/capacities, scheduled consumers, suspended include pins,
ordered replay spans and potential retirement checkpoints. The separate
[unit observer](../../native/motorola68000/amigaos/debug/opforge_lazy_units.asm)
records shared block-flag publication, capture/origin/line provenance, original
prepared offsets and actual ordered-copy translations. It reconciles complete
block ranges, preceding-entry ownership and numeric entries against the existing
block index, then records its selection state. Stored tables contain offsets and
scalar identities, not borrowed process pointers.

This inventory is observed **at prepared-record publication**, not independently
discovered from unbound acquisition records. Existing shared A8 head classification
is counted without adding a parser. Individual member/proxy/schema links and full
generated invocation identities remain deferred. Consequently, this experiment
does not establish that its inventory is sufficient to skip body preparation.
Generated whole units are reported separately when their publication has an
expansion context; generated names inside an ordinary unit are not separately
inventoried.

Lifetime predictions combine scheduled last consumers, successful replay/drain,
independently copied plans/template bodies/previews/provenance, and zero suspended
parent pins. Unscheduled child include regions remain retained. A later lookup
of a supposedly closed region invalidates coverage. Nothing is actually freed.
Main/unit errors, exhaustion, unfinished publication nesting, missing observation
boundaries and unbalanced cursors cannot supply a complete audit verdict.

Bounds are 256 regions, 256 scheduled mappings, 2,048 indexed units and 4,096
publication events. Only the first 16 rows of each detail group are printed;
complete aggregates cover the bounded tables. Detail truncation is distinct from
table exhaustion. The tables use 322,700 linked BSS bytes and allocate no heap.
Their fixed cost is appropriate for an opt-in observer, not a production inventory
layout. The canonical block-row include lives in the existing shared debug include
root, retaining block-index ownership; no extra include search path is required.

### Shared discovery repair required by the new source

A fresh current-source self-build exposed an existing discovery bug: an instruction
operand `.end` inside a captured macro was mistaken for a module terminator. The
actual audit header in separately discovered imported modules reproduces the same
failure; renaming only that operand removes it. A permanent control retains the
original spelling. Indexing and configuration now preserve template ownership
through its opening, body and closing records, independently of guard activity.
They retain the enclosing module's captured ranges and defer body semantics to
replay. The ordinary reference and audit images share this repair.

A distinct legacy capture ambiguity for `.endmacro`/`.endsegment` parameter
operands remains a later opcore parity issue. A8's current head contract covers
`.statement`/`.endstatement`; it does not replace legacy macro/segment roles.
No new head grammar or CPU-specific classification is introduced by this audit.

### Identical full input: runtime overhead

All six fresh full-input runs exit zero and match the entire live Rust Hunk:
198 files, 2,614,471 bytes, 708,592-byte output. Both bootstraps embed 68020 and
6502 packages; the frozen output embeds 68020. The frozen input is the previous
fragment-enabled source snapshot. This is full **fixed-input comparison**, not
a new audit-enabled current-source self-host qualification.

FS-UAE uses the existing A4000/68040 profile: 2 MiB chip, 8 MiB fast,
64 MiB Zorro III and 16 MiB graphics. It does not qualify the 2 MiB product target
or A6000 hardware performance.

| Serial release pair | Reference seconds | Audit seconds | Difference |
|---|---:|---:|---:|
| Reference then audit | 102.792439 | 107.690345 | +4.76% |
| Audit then reference | 104.140757 | 105.340650 | +1.15% |
| Two-run mean | 103.466598 | 106.515498 | +3.048900 s / +2.95% |

These are uninstrumented timings of the audit work. The small sample and
order-dependent spread limit precision; there is no optimization gain to claim.
Final release image bytes increase 747,492 → 750,348; linked reservation increases
781,264 → 1,106,596 (+325,332), including the tables. These comparisons use
the same final source with the shared discovery repair in both executables.

### Separate identical-input memory pair

| Tracked/linked resource | Reference | Audit | Difference |
|---|---:|---:|---:|
| Tracked heap peak | 47,587,208 | 47,587,208 | 0 |
| Allocated and freed capacities, each | 112,389,800 | 112,389,800 | 0 |
| Terminal live heap | 0 | 0 | 0 |
| Linked reservation, memory build | 794,220 | 1,120,196 | +325,976 |

Both memory runs report zero profiling errors and allocation failures. The audit
cost is linked storage/code, not an increased tracked heap peak. Combined linked
reservation plus tracked heap peak increases by 325,976 bytes; this still excludes
OS allocations, stack and allocator metadata.

The full audit links all 1,415 indexed units to 1,415 ordinary publication events,
with zero unmatched units and zero observer errors. It records 162 completed
scheduled spans, 164 regions, 114,504 physical captures, 114,626 capture lookups,
and a post-member-completion name count of 54,536. Those names are not individual
binding proofs. No runtime include acquisition occurs on this particular input.

Captured storage is 23,155,688 used / 32,452,608 reserved bytes. All 164 regions
become potentially retireable by the end of replay, with zero later-access
contradictions. Printed replay checkpoints show earlier eligible capacity. This
does **not** mean saving the full reserved capacity at peak: acquisition already retains those regions,
and the reference already releases them when replay finishes. A real lifetime
candidate needs a new measurement of overlap and actual heap peak.

### Proof and reproduction

Thirty focused fresh native runs compare reference, audit and isolated component-probe
images against live BIN/Hunk oracles. They cover both guards, nested/preceding/
internal labels, transitive imports and forward members, schema/configuration,
loop shadowing, generated names, include suspension/resumption and an imported
header whose template operand must not terminate discovery. The component
probe checks register/CCR/stack preservation, repeated reset, bounds/exhaustion,
overflow, failed mapping publication, remapping, closure and later-access detection.
Generated-unit lineage is a separate focused case; unresolved relationships are
not promoted to execution visibility.

The final-source default release self-build is separately proven: 198 files,
2,604,248 bytes of source and a 706,956-byte Hunk. Fresh FS-UAE START/DONE, exit
zero and complete live Rust equality are recorded at
`/tmp/opforge-lazy-l1-current-native-v5-20261014/result/native-result.json`.
The native executable reproduces its own complete bytes in 103.083183 seconds
on the same profile. This is a current-source self-host check, not an audit-enabled
self-host or an A6000/2 MiB qualification.

Runtime/memory receipts and original captures:
`/tmp/opforge-lazy-l1-full-v4-20261014/summary.json`. Final source images and
identities: `/tmp/opforge-lazy-audit-images-v5/images.json`, native source digest
`fnv1a64:e12b0b7c9be4fe32`. All five final images are byte-identical to the
measured and native-tested v4 images, including the shared discovery repair.
The only subsequent source change removes trailing whitespace from two empty
macro declarations by adding comments. `final-image-equivalence.json` records
the complete image comparisons. The exact final source was self-hosted freshly.

The [comparison runner](../../scripts/performance/run_lazy_preparation_comparison.py)
validates frozen file and image digests, actual build defines, and fresh whole-output
proof. Its [audit decoder](../../scripts/performance/lazy_preparation_audit.py)
reports coverage separately from native completion and memory measurements.

```sh
OPFORGE_LAZY_SAVE_DIR=/tmp/new-l1-images OPFORGE_LAZY_SAVE_PROBE=1 \
  cargo test -p asm native_lazy_audit_save_images -- --ignored --nocapture --test-threads=1

# Use the maintained FS-UAE environment; preserve the test binary outside target.
python3 scripts/performance/run_lazy_preparation_comparison.py \
  --workload-case /absolute/frozen-full-case/case.json \
  --images /tmp/new-l1-images --output /tmp/new-l1-results \
  --test-binary /absolute/asm-tests
```

Keep the audit opt-in. Its strongest next implementation opportunity is the
conservative scheduled-region retirement boundary; measure its real effect before
accepting it. Acquisition-only inventory remains necessary before deferred body
preparation, and that preparation work has not been avoided by L1.
