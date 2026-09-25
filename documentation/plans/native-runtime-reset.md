# Native runtime direction and migration plan

Status: active direction. The compact binary-source runtime now has a separate
provisional Shell executable, but it implements only a bounded language subset and
is not yet the normal native CLI path. Its current representation, measurements and
reproduction details are in the
[binary-source runtime note](prepared-source-experiment.md). Git history contains
the completed W1–W3, R1 and M1–M8 investigation records.

The active [operating contract](../../AGENTS.md),
[workflow](../workflow/README.md),
[native assembly guide](../../agents/rules/native-68000.md) and
[native parity contract](../../agents/rules/native-rust-parity-porting.md) govern
the work. This document records the current direction and active migration order;
it does not grant authority for future steps by itself.

## Product goal

The native assembler should be able to assemble itself on any Amiga with a 68020,
AmigaOS 3.1 or newer and 2 MiB installed RAM in at most 15 minutes, preferably much
faster. Qualification must include source loading, preparation, layout and output
in a runnable executable. Emulator results remain development evidence until a
baseline clock, storage and physical machine are chosen and measured.

Optimize the shared VM-based design across supported source targets. The 68020 is
the execution-platform floor, not a reason to move CPU-family semantics into the
native core. Rust remains the executable semantic reference and fast measurement
laboratory. Native layouts should use simple bounded memory blocks suited to the
platform rather than mirror Rust data structures.

Canonical packages remain authoritative. A derived runtime package may later
trade preparation time, size or CPU-specific layout for execution speed, but it
must be reproducible from the current canonical package and may not become a
second hand-maintained source of target semantics. Before 1.0, update producer and
consumers together and retain only the latest bytecode or package contract.

## Current architecture direction

Use controlled replacement: keep the existing native CLI as a correctness
reference while a compact path assumes one complete responsibility at a time.
Each increment must assemble a meaningful case end to end, compare with live Rust,
and identify which old work it makes unnecessary. Once the replacement is
qualified and integrated, remove the superseded path rather than maintaining two
native products indefinitely.

The compact path has established these principles:

- Tokenization produces the authoritative binary source line by line. Later
  phases consume numeric identities, structured values and compact expression
  programs; source text may be reread only to produce diagnostics.
- Stored binary representations contain offsets or numeric IDs, never process
  pointers. Every offset has a declared base and validated bounds, so moving a
  block does not require patching it.
- Instruction identities are normalized package-owned IDs. Aliases share an ID;
  semantic qualifiers remain explicit and IDs are unrelated to machine opcodes.
- Symbol identity, scoped binding and pass-dependent value are separate. Immutable
  package/source preparation is kept apart from mutable layout, CPU state and
  fixups.
- Preparation scratch is released before assembly. Allocations use measured
  requirements and bounded growth, with transient overlap included in accounting.
- Conditional telemetry uses reusable macros and contributes no code, data or
  imports to release builds. Instrumented work counts and release timing are
  reported separately.

New assembly should use opForge structs, macros, lists, loops and other language
features where they improve clarity. Names inside a module should be short rather
than repeat module qualification. Compact code and small files are design results,
not line-count exercises.

## Breadth migration plan

The current priority is language breadth rather than another isolated hot-spot
optimization. Each item is an inspectable increment, refined from evidence before
implementation:

1. **F1 — implemented: named constants and required expressions.** Added `name = expression`
   for practical standalone routines. Resolve immutable constants at their pass-one
   definition and verify the same value in pass two. Earlier constants, labels and
   the program counter may be referenced; forward or otherwise deferred constant
   dependencies reject explicitly in this fixed two-pass increment. Existing unary
   `+`/`-` and binary `+`, `-`, `*` form the initial expression set.
2. **F2 — implemented: package-owned operand shapes and predicates.** Carry the structural and
   register and rejection predicates required by indexed/register operands through
   the capsule and native selection boundary. Prove the same package-owned decision as Rust; do not accept
   unchecked token pairs or add CPU-specific recognition to generic native code.
3. **F3 — implemented: bit operations and forward absolute constants.** Added `&`, `|`,
   `^`, `~`, `<<` and `>>` with canonical precedence, and resolved forward immutable
   constant chains that are independent of layout. Detect cycles and missing names.
   Keep definition-site PC/label-dependent constants source-ordered; broader
   dependency/layout convergence follows as a separate increment.
4. **F4/F5 — implemented: named blocks, namespaces and canonical labels.** Bind
   nested scopes, parent and absolute qualified references to final numeric
   identities during preparation, including forward local shadowing. F5 adds
   namespace reopening, typed closes and canonical column-one bare labels.
   **F6** extends preparation to sequential single-source modules, visibility and
   fully qualified public cross-module references. **F7** adds explicitly ordered
   physical files and module-local imports. Native discovery, dependency ordering
   and include lifetime remain later work; do not restore string lookup during
   assembly.
5. **Source expansion.** Encode macro, conditional and loop syntax once as binary
   records. Expansion and control flow that depend on layout, the program counter
   or symbols must evaluate against the appropriate pass state without falling
   back to source text. Preserve macro-instance and source provenance for binding
   and diagnostics.
6. **Structured language values.** Add structs, lists and the expression/value
   forms needed by representative sources, using compact runtime representations.
7. **General layout and emission.** Support sections, discontiguous origins,
   relocation/fixups and convergence-sensitive instruction selection without
   caching state-dependent results.
8. **Product integration.** Feed the compact preparation/execution path from normal
   native package loading and the CLI, qualify representative projects, then remove
   the old responsibilities it replaces.

Select cases for semantic coverage and realistic work, not because they happen to
fit the implementation. Use more than one source-target family where the boundary
is intended to be generic. A complete small case is preferable to a wide set of
helpers that cannot produce an artifact Erik can inspect.

## Module boundary before the next phase

The module-focused phase has established compact CLI execution for explicit and
file-derived modules, selected-file discovery and dependency order, common
`.use` forms and visibility, selected includes, reference-driven named blocks,
bounded mapped-section content, and scalar import-site parameters. Fresh exact
Rust/native cases include successful output and rejection of missing/ambiguous
imports, cycles, private names and invalid include paths. The host-prepared
package boundary remains; native package generation is separate.

Direct per-item `.use` aliases, implicit file identities, direct wildcard
availability, and scalar configured parameters now have focused Rust/native
cases. Conditional or generated module discovery, remaining include path forms,
and general section-map composition depend on broader language and layout work.
Do not claim full module parity while those forms are unsupported. The next
phase returns to measured performance and broader assembler language support;
revisit remaining module forms when those capabilities make them practical.
Keep the binary-source representation and numeric graph so later assembly passes
do not return to source strings.

Rust now binds scalar `.use ... with (...)` values as private symbols in the
imported module. It evaluates the expression in the importer from values known
at the `.use` site; forward values fail. The compact native path now evaluates
supported signed-32-bit scalar expressions from earlier module-scope `=`
constants and incoming parameters. It carries their values by numeric symbol ID
through preparation into assembly. Preparation now also filters nested `.if` /
`.else` / `.endif` records using known module-scope scalar values inside a
reached imported block. `.const`, compound values, loops, conditionals needing
assembly-time values, and expressions outside the compact VM grammar remain
broader language gaps. Do not claim full module or parameter parity yet.
The [prepared-source experiment](prepared-source-experiment.md#module-parameter-checkpoint)
tracks this parameter subset and its limitations.

## Performance and language re-entry

The [bounded compact-CLI baseline](prepared-source-experiment.md#bounded-compact-cli-baseline-after-module-work)
now covers an imported instruction/data routine and a two-map structured-data
control on both m6502 and m68000 packages. All four cases matched live Rust
output in fresh 68020 / 2 MiB native runs. These are single, small-workload
observations, not a self-host estimate or a measured optimization gain.
Instrumented preparation points first to tokenizer VM work in both workloads;
the mixed m68000 case also has substantial assembly time. Count tokenizer VM
operations and attribute instruction selection/encoding before changing either
path, then compare release builds on identical inputs. Keep telemetry conditional
and use the existing reusable macros.

The principal language blockers for larger representative sources are:

- Macro, conditional, loop and `.statement` expansion, including provenance and
  pass-dependent values. The [opcore examples](../../examples/opcore) exercise
  these shared-language forms; they do not belong in CPU packages.
- General section/output layout and binary inclusion. The
  [AmigaOS raw-image example](../../examples/manual/motorola68000/amigaos/rawimageview_320x256x4_incbin.asm)
  needs section attributes, `.incbin` and `.output` beyond the bounded mapped
  sections already supported.
- String and richer data values, followed by broader expression and deferred
  layout behavior. These remain shared-language work; their exact order should
  follow a small complete source that can be assembled and inspected.

Base instruction rows for the two measured packages have not been shown to be
the main breadth blocker. The next implementation choice should be made from
the measured work counts and a concrete language case, preserving the normal
native CLI as the reference while the compact path grows.

The [full/compact CLI comparison](prepared-source-experiment.md#full-cli-comparison-attempt)
now completes with exact output for an M6502/M68000 common-subset module/import
workload. After the Rust Hunk-entry and M68K reference fixes, the full CLI took
about 5 seconds for eight M6502 blocks, 13 seconds for 24 M6502 blocks and
20 seconds for 24 M68000 blocks. The compact CLI completed near the 20 ms
host-polling resolution on the same expanded emulator setup; this is a clear
large relative gain, but not a precise ratio or 68020 / 2 MiB proof. The old
full CLI still misassembles `block+1` and emits an unreferenced sibling block
on the richer test source, so that case remains a correctness probe rather
than performance evidence. The next measurements should separate startup and
package cost from per-statement execution and improve timing resolution for
short compact runs.

## Selected modules and reachable output

Native discovery now selects the requested `.module` from a
candidate file, including when that file declares other modules. Selection is by
module identity, not by file identity; two requested modules in one file must both
work. The entry file remains the search root, not an ordering shortcut. Selected
modules retain their physical source and include provenance. This increment does
not prune code or data inside a selected module.

The Rust reference now uses named `.block` boundaries as its removable units.
Ordinary labels inside a block, including fall-through targets, belong to that
block. Qualified references to the entry or an internal label include the
whole block, and references from anywhere inside it include dependencies.
Unowned code/data remain in a reached logical section. A section with no named
blocks remains whole. The selected source is laid out and encoded at its final
mapped address before its bytes are appended; the old label-range byte copier
has been removed.

Focused Rust cases compare final branch and address bytes with directly placed
source, cover 68000 and 68020 layout, unowned bytes and references, and reject
a mapped section that exceeds its region. A selective `.use dep (entry)` makes
the named import available and validates it; it does not retain `entry` without
a reference from reached code. References inside discarded blocks do not retain
their targets. Native block pruning and mapped-section behavior have focused
fresh Rust/native parity for one and two imported maps in adjacent regions.
Rust replay reuses prepared lines when cached, but the native implementation
must operate on binary source records rather than reopening source strings.
Multiple concrete targets for one logical section, and multiple logical
sections targeting one concrete section, currently fail explicitly; ordered
multi-source mapping needs its own bounded increment. The current two-map cost
is recorded in the [bounded scaling case](prepared-source-experiment.md#bounded-two-map-scaling-baseline).

Native preparation preserves named `.block` open and close markers as bits in
each packed line's flag byte, then indexes offset-based spans after numeric
binding and module ordering. The experimental selector retains entry-file
blocks, follows numeric references into imported blocks (including internal
labels), and skips unreachable imported records before its two assembly passes.
Unowned code/data remain. The native import parser accepts unqualified selected
names in `.use dep (entry, helper)`, optionally followed by `as alias`. It
validates each name even when unused, but does not retain a block until reached
code references it. Repeated names within one list share a numeric selection.
Direct per-item aliases such as `.use dep (entry as chosen)` now bind the exposed
name to the selected original target; Rust and native reject combining a
per-item alias with a module qualifier. Wildcard imports remain outside this
bounded native path.
The compact CLI has one- and two-region section placement with focused live
FS-UAE parity. For the first imported map, native assembly
sweeps the packed records for concrete content before the imported logical
section while retaining selected-block pruning. Keep other unsupported mapping
cases explicit rather than silently assembling them in dependency order.
It also accepts two concrete sections placed in adjacent literal regions when
their emitted bytes form one contiguous image. A forward label reference across
those sections matches Rust in a fresh 68020 / 2 MiB run. Two imported maps
with distinct targets now use indexed two-slot section state. Native execution
follows pairwise concrete-then-logical ordering and matches Rust; same-region
overlap rejects. General placement ordering and sparse output remain
unsupported. The [bounded scaling case](prepared-source-experiment.md#bounded-two-map-scaling-baseline)
records the cost of its five packed-record sweeps per pass.

## Increment contract

For every increment, state the hypothesis, reference behavior, resource budget
and stop condition. Preserve a working reference path during the experiment.
Correctness evidence includes fresh native completion, explicit zero or expected
failure exit, exact live-Rust artifacts and cleanup checks. Production behavior
must never depend on a fixture, benchmark name, path or expected result.

Measurements use focused, reasonably complex inputs. Do not run the non-completing
native self-host case or extend timeouts to obtain a result. The existing limits
are 10 seconds after guest `START`, 60 seconds per invocation and 150 seconds per
batch. Report source and binary-record bytes, executable and linked size, retained
and peak owned memory, preparation work and uninstrumented elapsed time. A timeout,
launcher success or partial capture is not completion.

At each coherent checkpoint, keep the tree runnable, record remaining limits and
make a local recovery commit. Broad qualification belongs at meaningful integration
boundaries. Pushes remain separately authorized.

## Current decision boundary

F1 and F2 cover complete standalone byte-reversal, page-copy and range-check
routines, plus indexed-address and register-pair boundary cases. These are
representative small sources, not full-application or native CLI qualification.
The [runtime note](prepared-source-experiment.md) records current proof and costs.

F2 adds canonical package predicates for indexed operands and lowers the required
register names/classes into numeric runtime metadata. Native can exclude a
higher-priority rejection only when a known package register conclusively fails
one of its match predicates. Unknown names and unsupported predicates remain
fail-closed. Semantic operand encoding and table opcode emission both execute;
only a verified identity table is elided. The normal Rust m6502 route consumes the
same new package rows; other MOS CPU variants retain their existing specialized
selection routes until migrated and qualified.

F3 extends the expression and deferred-binding boundary with forward absolute
constants and explicit cycle rejection in both Rust and native. Native PC/label
dependencies retain fixed two-pass/source-order semantics. File discovery/includes,
expansion and product integration remain later increments. F4/F5 finalize named block
and namespace bindings during preparation and accept canonical bare labels.
F6 adds module ownership and public/private access checks; F7 resolves imports
across explicitly ordered files before assembly.
Select the next coherent breadth
step for review; do not start those increments automatically from this plan.

## F3 increment contract

Hypothesis: masks/shifts and forward absolute constants let practical routines use
clear symbolic configuration without repeated text parsing or repeated full-source
resolution passes. Index numeric definitions once, walk compact expression
references with an explicit dependency stack, and evaluate each absolute constant
after its dependencies resolve. Reuse symbol storage during this prelayout phase;
release bounded offset-based scratch before layout. Never publish an expression
offset or provisional value as a resolved symbol.

Prove complete small mask/configuration routines for m6502 and m68000 against
live Rust, including reversed declaration order, shared dependencies and signed
boundaries. Explicit negative coverage includes cycles (including PC-tainted
cycles), missing names, duplicate/colliding definitions, arithmetic range failure
and unsupported forward layout dependencies. Preserve earlier-label and
definition-site-PC behavior. Rust cycle handling is repaired as part of this work;
provisional cyclic values are not an oracle to reproduce.

The compact expression path retains its checked signed32 range. Operator
precedence and shift-count behavior follow the canonical Rust language; results
outside the supported range reject rather than truncate. Source/package lookups
remain numeric after preparation. New measurement code uses existing gated macros.

Baseline: F2 `6decb730`. Compare identical expression-layout workloads under the
same 68020 / 2 MiB emulator settings; CPU clock is not calibrated, so interpret
relative results only. Report complete routine output, release image size,
retained/peak owned memory and fresh completion/cleanup. Existing 10-second guest,
60-second invocation and 150-second batch limits remain unchanged. No self-host run.

Stop for review if this requires source reconstruction, a general layout solver,
full-source repeated convergence sweeps or a new unbounded storage structure.
Keep normal native package loading/CLI integration and other source-target
qualification out of this increment. Finish with a local checkpoint and an F3-only
review; no push is authorized.

### F3 checkpoint: reference defect and scope decision

Erik approved repairing production Rust before completing native F3 comparisons.
The repair resolves executed immutable `=` and `.const` scalar dependency graphs
with an explicit work stack, scoped/import-aware bindings and synchronized symbol
values/classification. Corrected values trigger the existing bounded layout
refresh; dependency depth does not require additional source passes. Already
correct absolute constants avoid a second expression evaluation.

The reversed 128-definition chain now emits `128,65,1` rather than `2,2,1`; the
pixel-mask routine computes mask `48` rather than `3`. Both independent byte
oracles and fresh native comparisons pass. Seven focused Rust regressions pass in
default and VM-only builds, covering definition forms, instruction widths/labels,
block/import binding, inactive definitions, mutable snapshots, PC/list values and
cycles. Production asm/core library lint passes.

The repair intentionally does not defer expressions that already error in the
initial pass or resolve structured/PC/label/mutable-dependent expressions as
absolute DAGs. See the [symbol contract](../opForge-reference-manual.md#36-symbols-and-assignments).

The full host assembler suite reports 1,544 passed, 160 failed and 35 ignored.
All 160 failing test names also fail with the frozen F2 test executable against
the same working tree (1,527 passed, 165 failed, 30 ignored). This is no-new-failure
evidence, not a passing repository qualification. The embedded package differs
from current package generation, causing equality failures and poisoned-lock
cascades; other baseline failures remain. All-target lint additionally reports
five findings in unchanged test/harness code. Do not regenerate packages or alter
baseline measurements to conceal these limitations.

Final-image bounded native proof now passes the pixel-mask and control-word
routines, the 128-definition chain, precedence/definition-site-PC data, retained
page-copy/indexed/register cases and all 82 compact/canonical evaluator cases.
Explicit native rejections pass for ordinary and PC-tainted cycles, missing
symbols, duplicates, label collisions, overflow and forward layout dependencies.
Fresh completion/exit/output checks and cleanup passed within existing deadlines.
The architecture guard now recognizes declared macro-parameter operands without
misclassifying them as labelled directives; its five tests and workflow checks pass.

Release throughput on identical expression-layout sources is approximately
unchanged in one pair (+1.3% m6502, +0.7% m68000), while image size grows by 1,508 B
and prior-case owned memory is unchanged. See the
[F3 measurements](prepared-source-experiment.md#f3-bit-operators-and-absolute-dependencies).
This completes the bounded F3 increment, not full native product or repository
qualification. Review before choosing the next breadth increment.

## F4 increment contract — implemented: named block scopes

Hypothesis: scope binding can be completed during preparation, allowing several
real routines to share short local names without introducing name lookup into
assembly or enlarging its per-symbol value state.

Support simple named `.block` scopes, `.endblock`/`.bend`, nesting, parent lookup,
absolute qualified references, forward references and scoped immutable constant
DAGs. A block declaration defines its entry label in its parent scope. A later
local declaration shadows an earlier outer declaration: do not bind a reference
permanently on first encounter. Match Rust's case-insensitive names and binding
rules. Anonymous blocks, dotted block declarations, namespaces, modules/imports,
includes and expansion remain explicit exclusions for this increment.

Preparation may retain source-name dictionaries and scope metadata. Assign
provisional numeric identities while streaming each line into binary records;
once declarations are known, resolve references and rewrite numeric identities
before freeing lexical scratch. Existing assembly and dependency evaluation must
consume final IDs only. Binary structures contain offsets and IDs, not process
pointers. Reuse the current 512 provisional source-name bound, 16 KiB name arena
and 256-byte packed-record limit; reject exhaustion rather than silently expand
limits. Report extra preparation storage and work through the gated framework.

Prove complete copy/fold routines for m6502 and control-word routines for m68000,
with repeated local labels/constants and qualified references. Each scoped source
has an equivalent flat fixture and independent byte expectations. Add a nested
lookup case covering forward local shadowing, parent constants, qualified names
and literal bytes that must not be mistaken for IDs. Negative checks cover scope
imbalance, duplicate definitions, sibling-name leakage, missing qualified names,
malformed closes and explicit unsupported scope forms.

Baseline: F3 `92a78722`. Measure unchanged 32-block expression-layout workloads
before/after, plus scoped versus flat routines on the final image. Use matching
frozen source/test producers, telemetry-off relative timing and separate gated
memory/work observations. Same 68020 / 2 MiB profile and 10-second post-start,
60-second invocation, 150-second batch limits; no calibrated clock or self-host
claim. Preserve the existing broad-suite qualification limitations.

Stop and discuss if this requires text replay during assembly, a general layout
solver, a package/CPU semantic change or a substantially larger language migration.
Finish with a local review checkpoint and current coverage/measurement notes. No
remote push is authorized.

F4 checkpoint: scoped/flat copy and control-word routines plus nested bindings
match live Rust and independent bytes on the native 68020 / 2 MiB path. All nine
scope rejection cases, the 512-ID boundary/overflow and the 128-definition chain
pass with cleanup proof. Two narrow Rust reference repairs cover forward local
scalar shadowing and unclosed lexical scopes. The full host suite has 1,548 passing
tests and the same 160 failing test names as the F3 baseline; focused default and
VM-only scope tests, production library lint and engineering guards pass.

One matched release comparison observes +3.7% m6502 and +1.8% m68000 time, with
1,940 B added to the image. This is language coverage with a modest observed cost,
not a speedup or full product qualification. See the
[F4 measurements](prepared-source-experiment.md#f4-named-block-scopes). Review this
checkpoint before selecting the next breadth increment.

## F5 increment contract — implemented: namespaces and canonical labels

Add simple named namespaces as the next scope slice: `.namespace name`,
`name .namespace`, labelled operand form, `.endnamespace`/`.endn`, nesting with
blocks and reopening operand-named namespaces. Match Rust's distinction: an
operand supplies a scope name, not an address symbol; a label still defines its
ordinary parent-scope address. Closing directives must match the opening kind.
Forward shadowing, parent lookup and absolute qualified references remain numeric
after preparation. Namespace identity may coexist with a value of the same name.

Hypothesis: reuse preparation-only scope metadata without increasing persistent
record or per-symbol runtime storage. Reuse gated preparation/accounting telemetry.
No original-text lookup during assembly, package changes or legacy executor. Keep
the current name/arena/record bounds. Dotted scope declarations, anonymous blocks,
files, modules/imports and expansion remain excluded; reject unsupported forms.

Prove a practical namespaced routine for each existing source family against live
Rust and independent expected bytes, plus reopening/mixed nesting/name-vs-value
contracts and malformed/duplicate/mismatched-close failures. Recheck retained F4
behavior. Measure unchanged expression-layout32 release workloads against frozen
F4 `11561735`, plus separate memory accounting on the new cases. Same 68020 /
2 MiB profile, 10-second guest, 60-second invocation and 150-second batch bounds.
No self-host or calibrated-clock claim. Preserve known baseline qualification
failures and finish with a local step-only review checkpoint; no push.

User clarification: exercise canonical bare labels directly. This increment also
normalizes column-one standalone labels and labels before instructions/directives
during preparation, preserving optional adjacent-colon forms and rejecting
indented labels. The prior compact path's colon-only restriction is removed;
reserved package spellings remain an explicit native naming restriction.

F5 checkpoint: both practical routines use canonical bare labels and match live
Rust plus independent expected bytes. The mixed namespace case, fourteen
rejections and two retained F4 cases pass natively with cleanup proof. Focused
default and VM-only host checks, native formatting and engineering guards pass.
No production Rust changes or new broad-suite qualification are claimed; the F4
baseline failures remain unresolved. One matched release pair observes +0.7% /
+0.1% time (below the polling interval), +360 B image size and no fixed-scratch
growth. See the [F5 measurements](prepared-source-experiment.md#f5-namespaces-and-canonical-labels).
Review this checkpoint before selecting files/modules or another breadth slice.

## F6 increment contract — implemented: single-source modules and visibility

Support sequential `.module dotted.id` / `.endmodule` regions, `.pub`/`.priv`,
existing blocks/namespaces inside modules, and absolute public references between
modules in one source file. Module ownership is distinct from lexical name
prefixes; dotted module IDs preserve parent-prefix lookup. Default visibility is
private, inherited on scope entry and restored on exit. Private module-owned
references are allowed only within their owning module, including forward
references; global labels retain Rust's global access behavior. A numeric ID used
from multiple modules must not conceal an illegal private use.

Hypothesis: bounded preparation-only ownership/reference metadata can finish
visibility checks before lexical storage is released, leaving assembly and
expression execution numeric and unchanged. Keep stored offsets/IDs, existing
512 provisional-ID/name-arena/record bounds and gated accounting. Measure any
extra preparation storage and timing. Preserve the non-module path and canonical
bare labels. Reject nested/duplicate/unclosed modules, unmatched closes, open
child scopes and program content outside explicit modules. `.cpu`/`.org` and
other program statements belong inside modules; `.end` may follow them.

Do not implement imports, `.use`, include/file loading, module metadata, sections
or expansion in this slice; keep explicit rejection. Prove practical routines
for m6502 and m68000, private internal/public external access, visibility restore,
forward cross-module references and dotted-prefix ownership distinctions against
live Rust plus independent bytes. Check private-access and malformed-boundary
failures with fresh native completion/error/cleanup.

Baseline F5 `5067b41d`; compare unchanged expression-layout32 release workloads
and separately account memory on module cases. Same 68020 / 2 MiB profile,
10-second guest, 60-second invocation and 150-second batch limits. Stop to discuss
if this needs text replay, unbounded reference logs, a package semantic change or
imports/file loading. End with current notes and a local step-only review; no push.

F6 checkpoint: module ownership and visibility finish during preparation with
three bounded side arrays (+3,084 B temporary storage), leaving 16-byte symbol
entries and runtime records unchanged. Practical m6502/m68000 module routines,
labelled directives, sixteen rejection cases and three retained scope regressions
pass on native: 22 functional checks. Canonical bare labels remain covered.
The 26 focused host tests and three VM-only module tests pass; no production Rust
change or broad-host qualification is claimed. Imports and multiple-file loading
remain future work. The matched release pair shows no meaningful regression
(about −0.75% observed time on both workloads); image size grows 1,068 B.
See the [F6 measurements](prepared-source-experiment.md#f6-single-source-modules-and-visibility).

## F7 increment contract — implemented: explicit source files and imports

Stream an explicitly ordered list of real guest source files through one native
preparation session. Each file has independent EOF/line numbering and complete
module/scope boundaries; declarations, module identities and bindings are shared.
Retain compact numeric file/record spans for diagnostics, with no stored pointers
or source text used during assembly. Existing one-file inputs use the same path.

Add module-local `.use module.id` and `.use module.id as alias`, including
forward availability within the owning module, case-insensitive qualifiers,
public/private access, duplicate aliases and missing/ambiguous module checks.
Resolve imports and references before freeing lexical scratch. Reuse the existing
512 provisional-ID limit and measure additional bounded metadata. Preserve bare
labels. Explicitly reject unsupported import forms and imports in child scopes.

The caller supplies assembly order. Native discovery, search paths, dependency
ordering, implicit file-derived modules and `.include` remain outside this slice.
The Rust file-graph oracle may order dependencies first; test manifests explicitly
match that order rather than claiming native graph loading. Source `.end` must
not swallow later files; keep the current terminal-marker restriction explicit.

Hypothesis: one shared preparation session can bind identical one-file and split
programs, with bounded per-file I/O/provenance overhead and no runtime string
lookup. Prove identical output against live Rust, exercise genuine guest file
opens, forward imports, alias reuse and private/missing references, and test file
boundary errors plus source-local diagnostic locations. Compare one/split release
times and gated peak memory, and unchanged expression-layout32 against F6
`36bc145c`. Same 68020/2MiB profile and 10s guest/60s invocation/150s batch limits.
Stop for a need to broaden language/layout semantics or restore source replay.
Finish with current notes, a local commit and step-only review; no push.

F7 checkpoint: real ordered source files stream through shared preparation;
module-local aliases resolve before assembly, including forward targets and
qualified-name interning order. File-local error locations use numeric spans;
final-binding/dependency error attribution still reports unknown. Native file
discovery, dependency ordering and includes remain deferred.

Eighteen native functional checks pass, plus joined/split and F6/F7 comparisons.
Host checks pass 30 binary-source, four VM-only and 45 runner tests. Joined/split
programs have identical output and measured memory peaks. Single release pairs
observe +39/+9 ms for splitting the small 6502/68000 fixtures and +1.9%/−0.2% on
unchanged F6 workloads. Cost: +2,476 B release image, +4,610 B preparation metadata
and a retained 12-byte numeric span per source (subject to allocation granularity).
See the [F7 results](prepared-source-experiment.md#f7-explicit-source-files-and-imports).

## F8 increment contract — experimental graph and discovery checkpoint

The entry file anchors search and identifies the initial modules to include;
its modules receive no exception from dependency ordering. Resolve every included
module's imports in source order, emit dependencies before their importers, and
include each module once. Reject cycles consistently, including entry-file cycles.
All modules declared in the entry file participate; unrelated modules in candidate
files do not join the graph. Update Rust and native together for this behavior
change; previous Rust entry-file ordering and preloaded-cycle exemptions retire.

The first reviewable checkpoint implements opt-in native graph execution over explicit candidate
files (first file is the entry), using numeric module IDs and packed-record spans.
Keep F7's explicitly ordered execution as the comparison path. Preparation captures
module boundaries and import edges once; ordering and assembly never reconstruct
or consult source text. Preserve file/line diagnostic provenance after reordering.
Native candidate preparation is initially eager: unused modules' missing imports
and unresolved values must not fail selected execution, but malformed/unsupported
candidate syntax can still fail preparation. This limit retires with selective
loading; it is not a new language requirement.

The second checkpoint adds native AmigaDOS directory enumeration from the entry
directory and additional roots. It scans `.asm` and `.inc` recursively, deduplicates
identical guest paths and prepares candidates for the same numeric graph. Search
does not choose the first match across roots. `.include` remains separate.

The initial discovery checkpoint eagerly prepared every candidate. The selective
follow-up now builds a declaration index and prepares only files in the requested
dependency closure. It skips unrelated invalid source, `.include` fragments and
duplicate unused declarations. The index remains syntactic. File-derived module
identity now works for files without explicit declarations; conditional or
macro-generated declarations and unused modules within a selected file remain
outside this experimental path. Rust's loader covers those cases. The scanner
bounds paths to 255 bytes, directory nesting to eight levels,
discovered files to 128, declarations to 512 and declaration names to 16 KiB;
an exceeded bound is an explicit failure.

Validation: live Rust output comparison for shuffled candidate order, diamonds,
entry-file dependencies, multiple modules per file and unused siblings; fresh
native rejection for missing modules and self/indirect cycles. Retain bare-label
coverage. Measure bounded release timing and allocation accounting against F7;
report feature cost, not an assumed speedup. No long self-hosting measurement.

The bounded 2 MiB diamond comparison is recorded in the
[prepared-source experiment](prepared-source-experiment.md#f8-numeric-module-graph-and-guest-side-search).
F8 added 3,220 image bytes and 2,964 linked reserved bytes relative to F7;
graph mode's conditional peak owned allocation exceeded explicit order by
25,088 bytes. The instrumented native elapsed times were effectively equal,
so F8 is a breadth checkpoint rather than a demonstrated performance win.

The selective follow-up adds 1,380 release-image bytes and 1,344 linked-reserved
bytes beyond initial F8. Its bounded 2 MiB proof and limits are recorded in the
[experiment note](prepared-source-experiment.md#selective-native-discovery-follow-up).

## F9 increment contract — selected-file includes

Selected native source files expand whole-line `.include "relative/path"` while
streaming through the binary frontend. Resolve the including file's directory
first, then the separately configured include roots. Nested includes share the
enclosing module/scope session; only the top-level candidate closes a file in
the module graph. Missing files, cycles and the eight-level include bound fail
within preparation. The unused candidate index does not expand includes.

Keep physical file/line provenance as numeric packed-offset runs. Rebase these
runs when dependency ordering copies module spans, so assembly diagnostics point
to the included fragment. No include path, source buffer or lexical lookup may
survive preparation. Preserve F7 explicit files and F8 discovery for comparison.
The current bounded checkpoint is described in the
[runtime note](prepared-source-experiment.md#f9-selected-file-includes).

This does not complete Rust's include language. Relative path components `.`
and `..` are now normalized for selected-file includes within allowed roots;
labels on an include line, preprocessor-generated includes and larger depths
remain outside this experiment. Broader language coverage should follow
focused parity cases instead of growing a parallel text preprocessor blindly.

## First CLI integration checkpoint

`opforge_compact PACKAGE.bsp3 SOURCE.asm OUTPUT.bin` is a separate 68020 Hunk
executable. It requires three unquoted, space-free positional Shell paths, reads a
host-prepared BSP3 package, starts from the specified source file, and writes
flat binary output.
The test harness and CLI call the same compact engine; the manifest-backed
multi-file path remains available for focused language tests. The CLI now accepts
`-M DIR` module roots and `-I DIR` include roots after the three paths. Either
option selects dependency-ordered discovery; the entry directory is searched
first, while the dependency graph decides output order. Without search roots,
the CLI keeps direct single-source processing for sources without modules. The
standalone CLI does not yet generate or load canonical packages natively or
replace `opforge_cli`. Unsupported input returns nonzero. Focused native proof
and current constraints are in the
[runtime note](prepared-source-experiment.md#first-compact-cli-checkpoint).

The first bounded layout increment adds same-name logical/concrete section
placement and a literal region to this compact CLI. It is limited to one
contiguous placed section; the focused identical-source Rust/native proof and
the intermittent 2 MiB emulator startup timeout are recorded in the
[runtime note](prepared-source-experiment.md#first-bounded-section-placement).

The next increment accepts one explicit `.use` section map between differently
named logical and concrete sections, provided the concrete section is empty.
Rust/native byte parity, native rejection of a nonempty mapped concrete body,
and image-size observations are in the
[runtime note](prepared-source-experiment.md#first-explicit-imported-section-map).
The remaining layout issue at that checkpoint was Rust's
concrete-before-import content order.

That ordering is now implemented for the single-map subset by two numeric
record sweeps per assembly pass. A concrete body with an imported reference,
unowned logical bytes and an unreachable imported block matches Rust under
the 68020 / 2 MiB profile; the [runtime note](prepared-source-experiment.md#concrete-content-before-mapped-import)
records the bytes and resource cost. Multiple maps and placements were a
separate scope decision at that checkpoint.

The two-map reference case exposed a Rust placement hazard: when consecutive
concrete sections share one region, late mapped growth can overlap the second
section. Rust now rejects that conflict. Two adjacent explicit regions produce
a valid contiguous reference image, recorded in the
[runtime note](prepared-source-experiment.md#two-map-placement-boundary).
Native two-map parity now uses indexed maps and section slots and schedules
both mapped pairs against their adjacent regions. Treat same-region repacking
after mapped growth as a separate convergence problem. Before broadening this
mode, use the [bounded two-map scaling baseline](prepared-source-experiment.md#bounded-two-map-scaling-baseline)
to track cost. On a 300-record case, instrumented assembly took 0.30–0.32 s
while tokenization took 2.18–2.20 s. Defer a span index until assembly scans
become material on a larger bounded case; investigate tokenization first.

## First binary `.segment` expansion checkpoint

The compact native frontend now captures a local, name-first
`NAME .segment parameter` body as numeric writer records and expands repeated
`.NAME expression` calls before the ordinary binary preparation path. Expansion
substitutes packed parameter tokens and never reopens or retokenizes source
text. The current bound is eight definitions, 4 KiB of captured records, and
64 bytes for one argument expression. An expanded record uses the invocation
line as its diagnostic origin.

This is deliberately a first subset: defaults, argument lists, nested
expansion, and imported segment visibility are not yet supported. The compact
CLI rejects unsupported forms; they need separate parity work before claiming
general `.segment` support.

A fresh 68020 / 2 MiB FS-UAE run matched live Rust CLI output exactly for
repeated instruction and data expansions on both m6502 and m68000 packages.
An unclosed definition returned a completed native error. In one bounded
eight-block mixed run, pre-change/current assembly time was 0.563/0.555 s
for m6502 and 1.150/1.186 s for m68000; these single observations show no
reliable speed difference. Hunk bytes rose from 48,580 to 50,024, and linked
reservation rose from 53,180 to 54,580 bytes. Dynamic peak owned memory was
not measured.

The next syntax slice also accepts `.segment NAME(parameter)` and
parenthesized `.NAME(expression)` calls. It still binds exactly one parameter,
and a parenthesized call must contain a simple, nonempty expression without a
nested parenthesis; multi-argument calls fail explicitly. Fresh native runs
matched the live Rust CLI for both spellings mixed with the existing bare form
on m6502 and m68000, and rejected a two-argument call with exit 20. In one
identical eight-block mixed comparison against the preceding commit, native
START-to-DONE time was 0.567/0.561 s (m6502) and 1.189/1.183 s (m68000).
The single observations do not establish a speed gain. The Hunk and linked
reservation each grew by 132 bytes.

Call-site labels now attach to the first expanded numeric record for both
`placed .NAME value` and `placed: .NAME(value)`. The label is emitted at the
invocation line's origin, and the generated record is marked as column-one
source so ordinary symbol handling accepts it. A first body record that already
declares a label is rejected in this subset. Fresh native runs on both packages
matched the live Rust CLI for subsequent references to both call labels;
the preceding unlabeled parity case still passed. An identical eight-block
mixed comparison showed 0.548/0.567 s (m6502) and 1.201/1.193 s (m68000)
before/after. One observation per package is insufficient to claim a speed
change. The Hunk and linked reservation each grew by 184 bytes.

## First binary `.macro` scope checkpoint

The compact native frontend now captures a name-first, one-parameter
`NAME .macro parameter` definition as numeric records in the same bounded
template engine as `.segment`. Calls accept `.NAME expression` and
`.NAME(expression)`, with an optional call-site label. Expansion injects
numeric `.block`/`.endblock` records to give every call its own scope and
rebinds definition-body source identifiers to that scope. Arguments retain
their call-site bindings; the captured body is never changed or read back as
source text. This supports repeated calls containing the same internal label.

Fresh 68020 / 2 MiB FS-UAE runs matched live Rust CLI bytes for three macro
calls, including a local label and a labeled call, on both m6502 and m68000
packages. The existing `.segment` parity case also passed after the shared
engine refactor. On an identical eight-block mixed workload without macros,
pre-change/current START-to-DONE times were 0.557/0.576 s (m6502) and
1.176/1.191 s (m68000). These are single observations and establish no
reliable timing change. Hunk bytes rose from 50,340 to 51,028; linked
reservation rose from 54,896 to 55,560 bytes. Dynamic peak was not measured.

This is still a bounded subset: eight definitions, 4 KiB captured records,
and a 64-byte argument expression. Defaults, multiple or zero arguments,
directive-first definitions, nested expansion, and imported macro visibility
remain outside this parity checkpoint. There is no source-text expansion
fallback for them.

The next macro syntax checkpoint adds zero-parameter name-first definitions,
directive-first `.macro NAME()` and `.macro NAME(parameter)` definitions, and
empty `.NAME` / `.NAME()` calls. The 68020 / 2 MiB native output matched the
live Rust CLI for these forms on m6502 and m68000. The earlier repeated
local-label macro case still passed. Argument lists, defaults, textual
substitution forms, nested calls, and imported definitions remain follow-on
work; this checkpoint is not general macro parity.

The next argument checkpoint accepts up to four named parameters and four
packed positional arguments in either definition/call spelling. It splits
commas only at the outer level of nested parentheses, brackets, and braces,
then substitutes both `.name` and `.1`–`.4` without reparsing source. The
same parser extends `.segment` argument lists. Fresh native m6502/m68000
output matched live Rust for both macro forms, nested argument expressions,
and named/positional body references. The earlier scoped macro and segment
cases also passed. This bounded slice requires exact argument count; Rust's
default and missing/extra-argument rules are not implemented yet.

The defaults checkpoint retains packed default expression tokens with their
definition, fills omitted parameters at each call, and permits extra
positional arguments through `.9`, matching Rust's substitution model. Supplied
arguments keep their call-site identities; default identifiers are rebound as
the expanded records enter the call scope. Macro lookup now checks numeric
scope ancestry so a definition in an outer block remains callable inside a
nested block. Real 68020 / 2 MiB runs matched Rust on both packages for
defaults, omitted/extra arguments, caller-block default resolution, and the
earlier repeated-local-label case. Current bounds are nine parameter/argument
slots, 192 packed argument bytes per call, and 512 default bytes per session.

An identical eight-block mixed workload without macros produced the same
output as the first macro checkpoint. One START-to-DONE observation per package
was 0.576/0.585 s (m6502) and 1.191/1.188 s (m68000), earlier/current;
there is no reliable timing conclusion. Hunk size rose from 51,028 to 52,332
bytes and linked reservation from 55,560 to 56,856 bytes. Dynamic peak was
not measured.

Nested `.macro` and `.segment` calls now use a bounded 64-frame stack of
packed invocation state. Generated records reenter numeric template lookup,
then resume their parent after a nested call drains. The live Rust CLI and
fresh 68020 / 2 MiB FS-UAE runs agreed for nested macro-to-macro and
macro-to-segment calls on both CPU packages; the earlier default-argument
and segment cases also passed. Recursion beyond the bound is an error.
The identical eight-block mixed workload still produced identical bytes.
One START-to-DONE observation changed from 0.585 to 0.547 s (m6502) and
1.188 to 1.173 s (m68000); these samples do not establish a speed gain.
Hunk size changed from 52,332 to 52,508 bytes and linked reservation from
56,856 to 57,028 bytes. Dynamic peak, including the invocation stack, was
not measured. The next parity gaps include textual placeholder forms,
imported template visibility, and string-bearing template records.

The next packed placeholder checkpoint adds a one-byte `@` token to the
shared tokenizer contract and expands `@1`–`@9`, `.@`, and `.{name}` from
captured binary argument records. `.@` contains the supplied argument list,
not defaults. Live Rust and fresh 68020 / 2 MiB native runs agreed for two
calls using all three forms on both packages; the nested-call regression also
passed. The identical eight-block mixed workload retained identical output.
One START-to-DONE observation was 0.547/0.550 s (m6502) and 1.173/1.189 s
(m68000), earlier/current; this is not evidence of a speed change. Hunk size
rose from 52,508 to 52,780 bytes and linked reservation from 57,028 to
57,296 bytes. Dynamic peak was not measured. This does not yet cover Rust's
textual concatenation around placeholders or strings; imported template
visibility remains a separate gap.

Macro substitution still needs exact argument spelling where a placeholder is
embedded in an identifier or string. A live Rust example with `symbol@1:` and
`"x@1"`, called with `A`, produces `symbolA` and bytes `78 41 00 20` at
`$2000`. Numeric package identity alone loses this distinction: the package
dictionary spells the same m6502 name `a`, and reverse lookup incorrectly
produced `"xa"`. That trial was rejected. A name-only spelling annotation is
also insufficient: Rust substitutes each trimmed argument's original text,
including numeric notation, punctuation and internal spacing.

The bounded exact-text slice now appends a flagged, offset-only argument
sidecar to call-shaped packed records during tokenization. Normal binding and
execution still use numeric tokens; no later phase rereads the source line.
The sidecar is stripped from ordinary directives. Macro calls split its exact
trimmed arguments, including omitted default spellings, and carry substituted
text through nested calls. Identifier recipes bind generated names to numeric
IDs; quoted template bytes expand in the packed body. The 256-byte record and
192-byte per-frame text limits reject overflow. Ordinary quoted data remains
literal, including `"x@1"` outside a macro.

Fresh 68020 / 2 MiB runs matched the live Rust CLI for package-name casing,
leading and infix identifier fragments, multiple placeholders, quoted strings,
numeric spelling (`$0A`), omitted defaults, empty calls, directive-first
headers, nested positional/named/full-list forms, and selected imported
macros. The eight-block repeated-macro workload independently checks 64 calls
and exact output. After the final quoted-template correction, one
START-to-DONE observation was 1.036 s for m6502 and 1.886 s for m68000;
linked compact reservation was 70,152 bytes. The unchanged eight-block mixed
binary-source workload retained exact output and measured 0.614/1.216 s; the
prior checkpoint's one-observation 0.550/1.189 s does not establish a stable
regression. Its linked reservation grew from 57,296 to 62,644 bytes. Dynamic
peak was not measured.

The same bounded exact-text rewriter now expands `.name`, `.1`, `.{name}` and
`.@` inside quoted template strings. Fresh Rust-oracle/68020 comparison covers
all four forms in one call, irregular spacing in `.@`, and an ordinary quoted
string outside a macro that must remain literal. Quoted supplied arguments,
quoted defaults, and a directive-first definition followed by a name-first
definition also matched the Rust CLI. A fresh nested `.@` case retained its
original comma spacing. These checks close the immediate quoted-template gap.

Macro work pauses here. Remaining parity issues include dotted embedded
substitutions in identifiers: a Rust-accepted macro with `a.suffix:`, `b.1:`,
and `c.{suffix}:` failed closed in the compact CLI at invocation; composite
identifier recipes currently cover `@1`–`@9` fragments only. The sidecar also
does not retain trailing whitespace after the last invocation token, and
quoted-string substitution after TKVM escape decoding may differ from Rust's
text-first substitution for escape-sensitive arguments. The 256-byte record,
192-byte per-frame text, four-argument and 64-frame depth limits remain
experimental bounds, not full macro-language parity. Review the growing
template module's responsibilities before widening support further.

## Compact CLI self-host convergence

Use the experimental compact CLI's own entry
`native/motorola68000/amigaos/experimental/opforge_compact_cli.asm` and its
transitive source files as the guiding workload. The authority is a fresh Rust
assembly of those same source bytes into the entry's declared Amiga Hunk. A
native result qualifies only after a fresh 68020 guest completion with zero
exit and a byte-for-byte identical Hunk. Do not count the host-built bootstrap
Hunk as a native assembly result. Keep the old full-CLI non-completing self-host
test out of routine measurement.

The initial Rust baseline builds a deterministic 58,856-byte Hunk at
`build/opforge_compact` from this entry (two separate temporary output roots
produced SHA-256 `fcf86df103ba8b41b79c31a391acee127beb395dd43a8fd019ae0f01e39d3223`
at checkpoint `c97d9735`). The compact native CLI currently writes a flat
contiguous byte stream. Focused section placement works, but its restricted
section/layout and directive handling does not yet cover the entry's full use
of `.section`, `.res`, and `.output`. Broader compile-time and 68020 instruction
coverage must be established from actual failures. Some source filenames also
exceed classic AmigaOS component limits, notably
`binary_source_discovery_index.i`. These are concrete work items, not
reasons to replace the target with a simplified self-host fixture.

First establish a reproducible source manifest, runtime package, Rust Hunk
oracle and bounded guest invocation that reports the first native rejection.
Then add the smallest coherent language/output slice that advances the unchanged
entry and its dependencies. Keep exact small Rust/native cases for each new
boundary, and rerun the real entry to choose the next one. Output and section
work must ultimately produce the Hunk itself; flat bytes cannot qualify as
self-host parity. Review memory cost and responsibility boundaries as features
accumulate, especially before broadening the template or app modules.

Measure release builds with guest START-to-DONE elapsed time and exact-output
proof. In separate instrumented builds, use the existing gated telemetry
macros for preparation stages, assembly clock, work counters, packed-source
size, owned-memory peak and cleanup; never combine an instrumented duration
with release timing to claim a speedup. The compact CLI runner now enables this
record with `OPFORGE_COMPARE_MEMORY=1`. On the focused 51-byte smoke source,
one release observation took 0.508 s and reserved 70,152 linked bytes; the
instrumented run recorded 303,872 peak owned bytes, 0.14 s preparation and
0.02 s assembly clocks, and 75,404 linked reserved bytes. These values prove
the measurement path, not self-host feasibility or stable performance.

Keep early entry attempts short and fail closed; extend the guest time budget
only when a previous bounded run shows meaningful progress. Record the exact
last completed stage and first failure, not just the timeout. Recheck the 2 MiB
profile at each useful checkpoint. The eventual target remains a complete
self-assembly within 15 minutes on a 68020/AmigaOS 3.1+ machine with 2 MiB;
prefer much faster. If a new feature exceeds that budget, revisit its memory
layout and reuse before continuing parity breadth.

The first bounded entry probe now uses Rust's live dependency manifest rather
than staging every file in each search directory: 43 source/include files,
457,729 source bytes, a 149,066-byte m68020 BSP3 package and the 58,856-byte
Rust Hunk. A fresh 68020 / 2 MiB native run completed with exit 20 and
reported `file 1, line 8`: `.section entry, kind=code`. This is an expected
readiness failure, not self-host parity. The separately instrumented run reached
the same line, recorded 656,128 peak owned bytes, balanced all tracked
allocations on cleanup and marked preparation incomplete. A valid full-command
native duration is unavailable at this failure point; the phase clock has no
completion stamp. The next bounded slice should carry section kind through
numeric records and verify the layout with a focused Rust/native case; a
following output slice must build exact Hunk bytes from the resulting sections.
Simply accepting `kind=code` and continuing flat output would conceal this
self-host target's four-segment, 70,152-byte linked Hunk requirement.
