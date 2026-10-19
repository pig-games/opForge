# Module preparation and relocatable objects

## Current status

A broader private-module CLI path is implemented as an **unpromoted checkpoint**. It
consumes eligible dependency-complete captured modules, publishes canonical public
interfaces, retires temporary private frontend/records, then links saved fragments
through ordinary global layout and assembly. Unsupported modules retain the complete
reference path; `--reference-modules` provides an explicit control.

The same-image full memory comparison accepts 15 modules and matches all 729,140 Rust
bytes, but peaks at 20,633,680 B versus 19,364,448 B reference. It fails the required at
least 1 MiB net peak reduction. The single release pair regresses from 124.115876834 s
reference to 256.462333459 s private (2.07×); current-source full self-host passes fresh
exit zero with 760,176 B equal to Rust and its bootstrap. This is not promotion,
activation on first actual reference, persistent `.ofm` caching or an 8 MiB fit claim.
Parity remains paused; the confirmed process/OS split is 6.5/1.5 MiB. See the
[measurement report](native-performance-diagnosis.md#current-assignment-and-decision-points).

## Implemented ownership boundary

Dependency-first publication preserves source order, configuration, whole-block
fallthrough and normal reachability. Successful publication owns its result before graph
acknowledgement and capture retirement. Ordinary fallback retains shared
frontend/template execution and definition-relative include paths; active file effects
happen once.

The private producer supports ordinary code/data/BSS sections, mandatory owner-zero
fragments, nested whole blocks, exact source label identity and explicit alignment
constraints. Layout retains alignment recipes rather than freezing original padding.
Fragment addresses carry fragment identity plus offset; equal original PCs do not
collapse distinct labels.

Canonical interfaces contain each declared public name once, with module ownership, and
optional nameless schema dependencies. Scalar/list/range/schema/nominal/member closure
remaps through numeric local-to-global identities. Values transfer into one retained
global semantic owner; saved code images discard duplicate semantic payload after
publication. Hidden liveness wrappers stay distinct from public address symbols, and
numeric ownership records preserve imported reachability. Serialized references are
bounded offsets/identities, never process pointers.

Imported symbolic addresses remain distinct from absolute constants across resets and
dependency analysis. Package-owned relative recipes replay at final source and target
placements. Fixed absolute field recipes retain numeric relocations only when generic
geometry proof permits them. Encoding and shared VM/package services own widths, grammar
and CPU semantics; the CLI does not decode instructions.

The producer requires an unconditional package reset prefix and retains that exact
numeric reset before its fragments. Other package-state controls, custom text registry
definitions, warnings, public templates, unsupported closure and unresolved
control/layout observations fall back. Earlier text-registry effects conservatively keep
later modules on reference replay. Unsupported work must not partly publish or repeat
file effects. Failure after namespace publication remains an explicit error with owned
cleanup.

## First-reference module assembly proposal

The intended endpoint remains an in-build near-output module image assembled on its
first **actual reference**. Current dependency-first configured-module attempts do not
implement that activation boundary. Persistent fingerprints and cross-build reuse come
later, after an in-build lifetime saving is demonstrated.

Actual references include exported symbols, typed definitions and template consumers.
`.use` discovery alone does not make every block reachable. Inactive branches and
unexpanded templates do not activate their contents. Mandatory module-level effects and
validation retain their order. Referenced whole blocks include internal labels and
fallthrough; macros/segments are templates, not block elimination units.

A complete image owns fixed payload and every remaining binding, layout, observation and
dependency recipe. Conditions, repetition and relaxation need convergence, not merely
byte patches. Configuration, declaration context, visibility, member provenance,
encoding state and definition-relative include paths must survive private retirement. A
published image cannot borrow frontend pointers. Unsupported semantics preserve the
reference engine or reject an explicitly experimental request; they never silently
freeze an unresolved observation.

The useful memory bound is shared runtime/package plus one active module and its
required closure, retained public interfaces/templates, compact payloads/recipes and
bounded readers. Cycles may enlarge the active set; it must not quietly retain every
module's old prepared state. Disk images help only when private owners retire before the
measured peak.

## Qualification and evidence limits

Focused native checks cover canonical values and schema/member closure, moved images,
exact label identities, code/data/BSS, alignment, imported absolute references,
source/original-image retirement, repeated owner resets and carrier reuse. Corrected
full geometry matches 79 live Rust bytes across 11 fragments. An unchanged real
`binary_text_names` module passes actual CLI binary/Hunk parity with accepted 1 versus
reference 0. All five final-image state/warning/placement/include fallback cases pass both policies
with accepted 0 and exact live Rust binary/Hunk output. Warning/include effects occur
exactly once; cleanup is enforced.
Component success is not a full memory result.

The current full/small/macro same-image MEMD comparisons all complete fresh exit zero,
match their live Rust oracles and end with zero ownership. Full packed records shrink
only 37,797 B while peak and allocation traffic rise. Exact admission is 15; there is no
proven complete rejection breakdown. Instrumented preparation counters are not release
timing. The single release pair completes exactly but regresses 2.07×; current-source
self-build passes with 760,176 B equal to Rust and its bootstrap. No statistically
resolved speedup is claimed.

Older unrestricted A6000 observations on another frozen input were 123/124 s resident
versus 1,292/1,254 s disk. They motivate sequential near-output ownership, but do not
predict this design's speed or establish 8 MiB fit. The process budget remains 6.5 MiB
with 1.5 MiB reserved for AmigaOS.

## Next coherent integration boundary

The current integration already supplies canonical interfaces, address/member remapping,
ordinary data/BSS and alignment. These capabilities do not satisfy the resource gate by
themselves. The planned next step is to finish bounded qualification and checkpoint,
then review the failed memory gate and large runtime regression with Erik. No further
implementation is planned before that course review.

The confirmed scheduling difference is a candidate explanation, not a measured byte
decomposition: private publication drains the final READY cohort while the full
configuration frontend remains live; reference final replay retires that owner first.
The initializer still consumes configuration names/import snapshots, so earlier
retirement needs an explicit compact ownership/remapping design. Limited admission and
active compiler overlap must be sized before forecasting net savings.

Any later approved change must preserve one reference engine, package semantics,
whole-block reachability, ordered module/file effects and explicit failure cleanup.
Measure identical full/small/macro inputs, separate release timing from memory, and
qualify current-source self-host before promotion. The [resource
decision](native-performance-diagnosis.md#private-module-cli-capacity-gate) and [capture
policies](native-lazy-preparation.md) remain authoritative.
