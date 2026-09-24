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
their targets. Native block pruning exists in the experimental single-PC path,
but fresh parity with Rust's mapped-section behavior is still pending.
Rust replay reuses prepared lines when cached, but the native implementation
must operate on binary source records rather than reopening source strings.
Multiple concrete targets for one logical section, and multiple logical
sections targeting one concrete section, currently fail explicitly; ordered
multi-source mapping needs its own bounded increment. Performance measurement
is also pending before treating this as a fast path.

Native preparation preserves named `.block` open and close markers as bits in
each packed line's flag byte, then indexes offset-based spans after numeric
binding and module ordering. The experimental selector retains entry-file
blocks, follows numeric references into imported blocks (including internal
labels), and skips unreachable imported records before its two assembly passes.
Unowned code/data remain. The native import parser accepts unqualified selected
names in `.use dep (entry, helper)`, optionally followed by `as alias`. It
validates each name even when unused, but does not retain a block until reached
code references it. Repeated names within one list share a numeric selection.
Per-item aliases, wildcard imports, and section maps remain outside this
bounded native path. Focused live FS-UAE cases cover selection and binding,
not Rust's mapped-section output.
The experimental native path currently has only a single output PC and simple
`.use` imports; logical-to-concrete section mapping and placed-region checks
remain a separate, necessary boundary before claiming parity with Rust's
integrated output. Keep unsupported mapping cases explicit rather than
silently assembling all imported blocks.

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
duplicate unused declarations. The index remains syntactic: implicit module
identity, conditional or macro-generated declarations, and unused modules within
a selected file are outside this experimental path. Rust's loader covers those
cases. The scanner bounds paths to 255 bytes, directory nesting to eight levels,
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
and `..`, labels on an include line, preprocessor-generated includes and larger
depths are outside this experiment. Broader language coverage should follow
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
