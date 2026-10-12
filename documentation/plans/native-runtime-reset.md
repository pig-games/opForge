# Native assembler completion plan

## Assignment and completion order

Complete current Rust assembler language, packages and assembly CLI/output
behavior through the compact native path in this order:

1. Shared opcore.
2. 6502 family.
3. Complete 68000 family, including FPU, MMU, AMMX and 68080/Apollo.
4. i8080.
5. RISC-V.
6. 6809.

Repair discovered Rust defects before qualifying corresponding native behavior.
Keep CPU/family/dialect semantics in package owners and generic directives in
shared processing. Developer tools remain separate. Continue autonomously through
coherent recovery points; consult Erik for dramatic performance or memory
regressions. Begin no optimization rounds until this completion sequence is done;
then report remaining limits and wait.

## Current evidence and limits

The 2026-10-12 frozen-image correctness-only opcore audit completed all 86
unchanged roots under the fresh-run protocol: 32 positive roots match every
requested live Rust artifact exactly, 36 expected-negative roots reject with
diagnostics (not exact diagnostic parity), and 18 positive roots exit 20. All
cases have fresh completed protocol evidence. Compared with the prior combined
binding audit, four roots improve and none regress: `cond_syntax`,
`module_qualified_section_map`, `module_use_include` and
`section_module_use_include`. The unchanged `syntax.asm` now reaches sparse
`.org` emission on line 196; it still fails.

The 174.33-second correctness test includes host preparation and harness overhead;
it is not a performance measurement. The Rust reference audit matches 81 roots;
five expected-error roots lack comparable partial artifacts. An initial restricted
host launch failed before guest startup and supplies no native result; the complete
rerun used the required GUI/process access. Evidence and image/package/input
digests are in `/tmp/opforge-static-includes-full-opcore-retry-20261012.json`.
This does not qualify complete opcore or CPU-family parity.

The 18 remaining positive failures group by fixture: five statement signature or
expansion roots (`statement_boundary_span`, `statement_capture_types`,
`statement_cross_module_ok`, `statement_expansion`, `statement_signatures`); four
linker/output-policy roots (`linker_regions_full`, `linker_regions_minimal`,
`linker_regions_no_dsection`, `linker_regions_pack_no_dsection`); three metadata
roots (`module_metadata_block`, `module_metadata_output`,
`module_metadata_outputs`); and six roots (`cli_json_outputs`, `expr_syntax`,
`macro_syntax`, `module_basics`, `syntax`, `use_wildcard_import`). These are
observed failure groups, not established causes. Source/session behavior and
dynamic binding ownership still require focused controls outside this corpus.

The numeric proxy-ownership repair qualifies direct and composed imported
typed-member references: 18 positive controls match live Rust and two malformed
`@0` controls reject freshly on 68020 and 6502. Unknown/mutable prefixes retain
possible owners; a common owner is indexed once, while differing owners reject
only when reached. Runtime block marking remains O(1), with no new heap allocation.
A subsequent five-scenario matrix matches forward fields, forward compound
aliases and mutable-kind changes on both targets (eight exact positive cases).
The longer unknown prefix declared in another physical block still rejects on
both targets; native dynamic selection between those owners remains pending.
Rust now resolves actual declaration ownership, carries typed aliases through
layout stabilization, and preserves used finalized schemas without retaining
pruned code. Its focused member/schema controls pass; the native owner matrix
continues to require the valid different-owner case to succeed.
The current acquired-include regression passes all ten positive behaviors on
each package (20 fresh exact cases), including inactive composed block references.
That matrix is separate from the composed-member controls. Named acquired
collections pass twelve fresh exact controls, including colonless
and explicit-colon labels. The final candidate checks bounded batch publication
and independently validates marker nesting and balance in inventory and runtime. Actual assembly-state/
PC-dependent acquisition also remains pending. The identified proxy-repair image
is 657,484 bytes with 681,816 bytes linked static reservation. The subsequent
collection image is 659,608 bytes with 684,524 bytes linked static reservation; neither is peak RAM.

The latest full self-host proof is BS35, source manifest `54fb484dd0022ab5`:
121 mapped inputs (1,780,288 bytes), with exact bootstrap/native/live Rust equality
for the entire 601,196-byte Hunk. The uninstrumented release run took
1,522.60420225 seconds on FS-UAE 68020/74 MiB. This proves that identified source
checkpoint only. Later changes have focused evidence, with no new full self-host
proof. Full BS35 peak allocation, physical A6000 timing and 2 MiB fit remain
unmeasured. See the maintained [self-host proof](compact-frontend-vm-boundary.md#most-recent-full-self-host-qualification-bs35).

The separate shared text-owner component matches its complete live Rust artifact
in a fresh native run. This covers builtin/custom maps, variable escapes, cloning
and lifecycle APIs. The shared packed text-grammar component separately matches
its complete 384-byte live Rust artifact, including planned/snapshot record flags
and malformed-input rejection. Source directive/emitter and EXVM encoded-string
provider integration remain pending; component proof does not qualify those paths.

Source-session restructuring is in progress; its frozen-input lifecycle has
focused native proof and is now the sole assembly driver. It does not yet support
growing records or actual-state acquisition. Its isolated unchanged release
control adds 0.76% on one sample and 952 bytes linked static storage.
Independent symbol buffers now stage allocation and commit their views/count
atomically. Fresh component and production matrices each pass twelve exact
positive cases and two malformed rejections. Stress controls preserve cells and
pointers across all five injected allocation failures, support prepared keys and
lazy kinds, and exercise the 65,536-symbol bound. The unchanged release control
changes from 27.064863 to 26.944213 seconds (-0.45%, one sample), with 568 more
linked static bytes. Frozen session pins remain; actual-state acquisition,
relocatable source views and peak heap are not yet qualified.

Record-relative loop positions and refreshed owner views now pass twelve fresh
exact collection cases and two malformed-input rejections through the production
CLI. The unchanged release control changes from 26.848584 to 27.116166 seconds
(+1.00%, one sample), with 504 more linked static bytes.

Rust conditional declaration ownership is repaired in `aaca8086`; 98 focused tests
pass. Native adds recursive shared-EXVM certification/evaluation for immutable
scalar closures in private arrays, without publishing values globally. Four focused
fresh positives match exactly across both CPUs, including transitive references
and sign handling. The v3 feature control assembles 384 source bytes into 268 packed
bytes in 1.801896042 s of release execution. Separate MEMD telemetry reports a
641,792-byte peak, 1,031,784 bytes allocated and freed, and zero live bytes after
cleanup. The unchanged 1,200-call release control changes from 33.089063959 to
33.094098333 s (+0.02%, one pair; no measurable change).
The current native repair separates executed immutable declaration activity from
stored values. Ordinary-loop candidates no longer enter eager dependency indexing;
unknown owners defer, only completed inactive owners allow shorter-name fallback,
and an active owner retains its identity before value availability. Private scalar
observations preserve numeric layout history across sizing resets without retaining
compound pointers or granting immutable certificates.

The activity checkpoint's 112-case matrix completed on both CPUs: 62 exact
positive artifacts, 40 expected diagnostic rejections and ten mapped-section
failures. Those counts remain historical; the matrix was not rerun after the
mapped-flat repair. The 523.12-second batch is correctness-only and does not
qualify exact diagnostic text. Rust `7073f2de` reconciles inactive readonly
source symbols after completed sizing convergence.

The isolated unchanged release control takes 33.291479875 s versus BS37's
32.955889917 s (+1.02%, one sample each). Image and linked reservation grow 3,092
and 2,960 bytes. Separate telemetry increases unchanged-control peak owned memory
by 16 bytes; all allocations are freed. The activity feature control peaks at
690,392 bytes and exercises two observation passes and one restored scalar row.
[Measurement details](../performance/vm-efficiency.md#executed-declaration-activity-and-scalar-observations)
identify the frozen artifacts and limits. These `68020-10m` checks do not qualify
the 2 MiB product target or a fresh full current-source self-host.

The mapped-flat repair now passes all ten former mapped owner cases freshly and
exactly across 6502 and 68020. Seven mapped Hunk cases also match exactly,
including reversed output selection, dependency-first assembly and two logical
destinations; the Hunk BSS instruction-relocation regression is exact. The repair
uses actual-geometry slot lookup, concrete first-declaration ordering, logical
alias destinations and contiguous-flat selection, with BSS `initializedLimit`
kept separate from `size`. It adds no state memory growth. Map v2 and v3 outputs
are byte-identical. The v3 image is 678,120 bytes with 711,228 linked bytes
(+260 bytes versus activity v7). No fresh full self-host or 2 MiB claim follows.
The unchanged release control is 33.330524583 s versus 33.291479875 s (+0.12%,
one pair); separate MEMD unchanged-control telemetry is unchanged from activity:
1,282,360-byte peak, 3,182,136 allocated and freed, zero live, 27,022 packed
bytes and a 425,506-byte prefix. The 34.441206458 s instrumented run is not
release timing. See [mapped-flat measurements](../performance/vm-efficiency.md#mapped-flat-repair).
The earlier schema-replay-only comparison
(33.112641 to 32.804228 s, -0.9%, with 564 more linked bytes) predates this
repair. UTF-8 preservation in Rust
macro preprocessing is repaired in `a616140f`, with all 26 preprocessing tests and
all 211 opcore library tests passing. BS36 text emission/provider integration passes 52 fresh native controls: 24 exact
positive artifacts and 28 diagnostic rejections across 68020 and 6502. Its isolated
unchanged release control grows from 27.42 s to 33.11 s (+20.8%, one pair); linked
storage grows 13,464 bytes (+2.0%), and instrumented owned-memory peak is unchanged.
See [source execution](native-source-execution.md) for scope and measurement limits.
The latest qualified full self-host remains BS35; no complete opcore claim is made.

VM-contract consolidation remains a separate pending design.
Consolidate each VM's current capabilities into its own
v1 contract, remove superseded formats and native VM version fields/checks/dispatch,
while preserving package/container integrity and bounds validation.

The BS37 shared reservation slice adds `.ds count` through numeric directive
binding, shared expression preparation and assembly-owned output. Flat leading
and trailing reservations produce no payload; internal holes retain zero-filled
Bin storage and separate addressed Hex/S-record spans. Initialized sections retain
padding, while BSS uses the existing independent fragment/placement geometry and
produces no initialized bytes. Layout mode no longer selects Hunk serialization;
explicit source Hunk descriptors retain that policy. Both shared and separate
regions have focused proof. All 24 fresh reservation cases pass: 12 exact positive
artifacts and 12 expected diagnostic rejections across 68020 and 6502. Six final
Bin/Hex/S-record controls and the existing Hunk BSS regression also match exactly.
The complete unchanged `syntax.asm` still rejects, now at labeled `.if` line 280
instead of `.ds` line 114. This does not change the full corpus qualification claim.
The unchanged release control takes 32.955889917 s versus 33.094098333 s (-0.42%,
one sample per state), with 424 more executable bytes and 396 more linked bytes.
Separate telemetry reports only 16 additional owned bytes at peak and zero live
bytes after cleanup. See the [measurement details](../performance/vm-efficiency.md#shared-byte-reservations-bs37).
No new full current-source self-host or 2 MiB qualification is claimed.

The combined native forward/member matrix passes 16 fresh cases on 68020 and
6502: ten exact positives and six expected rejections, covering forward
scenarios, composed controls, and missing-member/scalar-prefix/longest-owner
negatives. Numeric member-plan acquisition runs after declarations in
`imports.finish`, retaining importing lexical/module owners; final frontend name
counts refresh before exported values, plans and member keys. Ordinary
contextual aliases in Rust still require their existing immutable certificate. The
combined frozen image is 678,952 bytes with 712,060 bytes linked; see the
[measurement record](../performance/vm-efficiency.md#control-labels-and-forward-member-preparation)
for separate timing and heap results.

Frozen source-policy v5 passes eight Hunk policy cases across both CPUs (two
exact long-relocation outputs and six expected rejections). Narrow source Hunk
references reject, as does CLI-only Hunk selection without source policy even
with mapped geometry. Flat geometry is independent from output relocation
policy; source `.output ...,format=hunk,sections=...` sets `Context.Relocatable`, with mapping
validation retained. The combined `68020-10m` labeled-control/address matrix passes 24
cases (22 exact positives and two expected diagnostic rejections), including
conditional labels, parent-active publication, named iteration records and
logical mapped sections, plus placed-word imported routine addresses at $0902.

The native source path remains experimental. `.statement` signature matching and
capture expansion are not implemented in that path; the historical corpus audit
does not establish those capabilities. The unchanged release control rises
separately by 0.73% for labels/output policy and 0.20% for forward preparation
(one sample each). Owned-memory peak increases by 16 bytes for the former and
stays unchanged for the latter, with zero live bytes after cleanup.
BS35 remains the latest full self-host
proof; these controls make no complete-opcore or 2 MiB claim.

## Current integration checkpoint

Rust match replay now records arm guards and executed ownership, including
forward case-local loop counts and parent-active control labels (`fae73e68`).
Mapped placement now reserves retained imported extents before later sections,
replays changed geometry and preserves the unchanged-layout reachability budget
(`3306a443`). Focused regressions pass. Full current host subsystem results are
211 shared-core, 101 package and 549 VM unit tests passing; VM integration tests
also pass. A stale authority test now mutates the effective CPU-owned table.

Native match handling uses one current eight-head shared branch program, typed
frames and existing guard observations. Configuration classification IDs now
occupy a disjoint private negative domain. The complete fresh 70-case matrix
passes 38 exact positives and 32 expected diagnostic rejections across both
packages. Preparation-only effects inside retained match envelopes remain a
separate language gap.

Scalar and typed expression capsules share bounded opaque framing across operand
transport. The upstream classifier now routes outer parenthesized comma children
through existing structural preparation, without assigning CPU meaning to them.
Seven tuple/displacement/product controls match freshly and two malformed cases
reject; four immediate/external-65C02 controls also match exactly.

Sorted disjoint emission spans and inserted-span coalescing pass all eight
Hex/S-record placement controls on 6502 and 68020, including the stronger
single-region mapped-growth cases. One pre-START emulator stall required an
isolated fresh retry; it is not native algorithm evidence.

Static included-source capture now acquires/indexes child streams before graph
ordering. The shared FilePlan selector owns syntax, the conditional owner decides
static acquisition, and the resolver/reader-suspension code is shared with runtime
acquisition. Eighteen exact positives and twelve expected rejections cover nested
paths, actual module search, included `.use` edges, selected-file siblings,
cycles, assets and two unchanged default-8085 examples. Runtime/expanded
module-graph changes remain rejected. Initial CPU bootstrap skips include
statements and balanced conditional regions; template definitions within such
regions remain a conservative initial-selection barrier.

[Separate release and heap controls](../performance/vm-efficiency.md#separate-follow-up-costs)
show no substantial tuple/framing/output regression. Static acquisition changes
the unchanged control from 33.567695750 to 34.875348041 seconds (+3.90%), with
16 additional peak owned bytes and zero live owned allocations on exit.
These are focused integration results, not complete opcore, a new full self-host
or 2 MiB proof. The complete current opcore audit is reported above.

## Immediate shared-language sequence

### Active statement slice

The five unchanged statement fixtures define the next structural gap. Move
signature compilation, typed token matching and definition-head policy into
shared PRVM services, retaining one immutable offset-based signature plan across
Rust imports and clones. Native preparation supplies normalized lexical facets;
it must not recreate signature syntax or instruction-specific register parsing.
Keep overload selection, literal adjacency, numeric ranges, letter/digit fallback,
caller labels and definition-relative provenance covered against the independent
core reference behavior.

First qualify the compiler/matcher components and package boundary. Then connect
numeric statement storage to the existing template cursor and substitution owner,
rather than adding another expansion engine. Component completion alone does not
add `.statement` support to the compact CLI. The new BS38 preparation programs
have no native VM-version fields or dispatch; this does not complete the pending
consolidation of older VM contracts.

The initial shared-service component qualifies 187 cases in one fresh native
run: exact live Rust plan/capture/status output, explicit zero guest exit and
completed protocol. It covers signature syntax, typed matching, head modes,
UTF-8 normalization and atomic capacity/budget failures. Start-to-completion was
5.054530 seconds on FS-UAE 68020/10 MiB; the 27.95-second host test also includes
assembly and runner overhead and is not CLI assembly timing. All five unchanged
Rust statement fixtures still match their Hex and normalized listings, including
cross-module use. Rust now retains invocation provenance for a label emitted by
an empty statement body; opaque preparation caches stay out of semantic equality
and AST debug representation.

Native template storage exposes a numeric external-definition facade, and a
separate adapter stages normalized facets from owned TKVM records. Invocation-prefix
selection, overload dispatch and generic letter/digit fallback have shared VM
services. The connected source/template orchestration is described below; these
earlier component proofs alone did not establish CLI invocation.
The adapter separately qualifies 96 cases against live Rust facet output,
including numeric suffixes and overflow, canonical punctuation, decoded/lossy
UTF-8, name IDs, bounds and atomic publication failures. Its fresh native
start-to-completion time is 6.335172417 seconds. This is the owned-record boundary,
not complete TKVM-to-statement CLI execution. Disabled statement telemetry builds
are byte-identical to removed calls under all three incomplete gate combinations;
enabled telemetry assembles and has static ABI-preservation checks, without a
claim of enabled native execution.
The native package boundary qualifies all 68 current cases through one frozen
validator and isolated case inputs: the valid package returns zero and 67
malformed packages return the exact expected nonzero status, each with fresh
completion and zero harness exit. The correctness test takes 105.96 seconds
including assembly and two emulator boots; it is not assembler timing. Two old
name-ID mutations now replace the complete 16-bit field with an out-of-bounds
ID, rather than relying on a low byte that became valid as the name table grew.

The unchanged release macro control changes separately from 34.697495584 to
34.898929042 seconds (+0.58%, one sample each); peak owned storage rises by
32 bytes, with zero live owned allocations on exit. Release image growth is
2,872 bytes and linked reservation growth is 2,832 bytes. See the
[foundation comparison](../performance/vm-efficiency.md#shared-statement-preparation-foundation-bs38).
None of these component/control results adds CLI statement invocation or a new
full self-host claim.

The invocation checkpoint qualifies 242 current shared component cases against
live Rust output, with fresh completion and explicit zero guest exit. A9 lives
in the current A7 matcher program; the superseded 125-byte program is rejected,
and the latest program is 133 bytes. Routing selects the longest visible keyword
prefix, preserves adjacent label-colon boundaries, and delegates ordinary versus
letter/digit fallback matching and overload ambiguity to shared services. Native
selection and derived-view capacities are checked independently and outputs stay
atomic on failure. The 11.954011083-second component run on FS-UAE 68020/10 MiB
is correctness timing for this expanded workload, not CLI performance.

Rust's exported invocation scope now retains the caller-visible qualified key:
`Q.PUSH` remains `Q.PUSH` rather than leaking unqualified `PUSH`. Selected aliases
and wildcard imports retain their respective keys and share immutable compiled
plans. All five unchanged statement fixture outputs still match; nine focused
assembler tests, 962 affected subsystem tests and Clippy pass. Source acquisition
and numeric selection have separate owners. Export/scope rows are still rebuilt
per Rust invocation; no optimization or broad statement parity is claimed.

The compact CLI now connects direct source statement declarations and invocations
to the shared services and existing packed template cursor. Ten fresh native
cases match live Rust output exactly, with zero guest exit and completed protocol:
byte/word overload selection, caller labels, empty and duplicate captures, named
and positional text substitution, decoded expression captures, numeric fallback,
keyword suffixes and independent macro/statement names. Definition storage,
caller-visible keyword exposure, invocation scratch and capture transport have
separate owners. Private statement carriers do not register ordinary macro names.
The temporary capture writer restores the live frontend frame used by binding
callbacks; new key arenas are allocated before exposure validation.

Eight additional connected same-source module-graph cases qualify actual `.use`
acquisition: qualified aliases, selected bare/renamed exports, wildcard/full
module qualification, later-import replacement with local overloads, and rejection
of private selection, implicit leaf aliases and sibling-block ambiguity. The five
positive cases match live Rust bytes; the three negative cases complete freshly
with guest exit 20 and diagnostics, without an exact diagnostic-text claim.

Calls inside expanded macro/segment/statement bodies now have ten focused fresh
native/live Rust comparisons. They cover forward callees, numeric/text formals,
keyword adjacency, literal numeric spelling, caller-module lookup from an imported
body, inactive calls, explicit body labels and inherited labels through an empty
statement. A separate plain macro case proves the packed path with no statement
definitions. This is focused coverage, not complete `.statement` or opcore parity.

Physical token acquisition retains optional whole-line recipes only within
template bodies (plus conservative dynamic acquisition). The existing shared
classifier tracks this independently of blocks, namespaces and configuration
activity. The template cursor uses optional recipes when a lexical consumer is
needed; otherwise it removes the private trailer and executes the packed body.
Required substitution recipes remain required. Each returned record explicitly
publishes lexical freshness and numeric inherited-label provenance. Dispatch never
reuses stale caller tokens or rereads original files.
The separate exposure component validates acquired import descriptors; the eight
connected cases are the current acquisition proof. This checkpoint does not change the BS35 full self-host
evidence or establish a new hardware/2 MiB qualification.

The unchanged release macro control separately takes 34.904874125 seconds before
this integration and 36.037844917 afterward (+3.25%, one sample each). The image
grows by 21,624 bytes and peak owned storage by 2,880 bytes (+0.22%) in separate
instrumented runs, with zero owned storage left on exit. These costs are isolated
from the earlier foundation delta; see the
[integration comparison](../performance/vm-efficiency.md#connected-statement-preparation-and-invocation-bs38).

The expanded-body slice separately changes the same release control from
36.030338375 to 36.227721417 seconds (+0.55%, one sample each), grows the image
by 720 bytes and peak owned storage on that control by 16 bytes. These small
control deltas do not measure recipe storage on a large template-heavy project;
retain that distinction when qualifying the full native source.

The subsequent frozen 86-case opcore audit qualifies 35 positive cases with
exact live Rust/reference artifacts and 36 expected negative cases with fresh
rejection/diagnostics (no exact diagnostic-text claim). Fifteen positive cases
remain: CLI JSON, expressions, four linker cases, macro/module syntax, three
metadata cases, three statement fixtures and general syntax. All current Rust
reference oracles pass. This raises positive coverage from 32 to 35; it does not
close opcore parity. The statement fixtures include comment-only/empty bodies
and ordinary labels/instructions, so they take precedence over new breadth.

Correctness requires fresh component comparison, unchanged local and cross-module
fixtures, malformed/ambiguous invocations and existing macro controls. Measure the
unchanged release control and a separate instrumented heap run against the saved
BS37 image/package before claiming the integrated cost. Keep current-module and
graph-changing source acquisition boundaries explicit.

1. Use the complete current opcore audit to choose the next structural language
   gap. Known candidates include statement signatures/expansion, metadata,
   linker output policies and sparse output. Keep CPU-specific failures in their
   family owners rather than retargeting examples.
2. Complete source/session gaps: actual assembly-context-dependent acquisition,
   runtime graph changes and unresolved dynamic binding ownership. Preserve
   strict unavailable-value rejection and numeric origin/cursor contracts.
3. Complete the separately approved one-current-v1 consolidation for each VM;
   remove native version fields, parameters and dispatch while retaining package
   integrity and bounds checks. The expanded match program does not fulfil that
   migration.

Each coherent outcome needs fresh positive and negative native controls, separate
release and memory measurements, and a review of numeric execution ownership.
Keep new responsibilities out of the already large frontend. Continue shared
language work before CPU families; optimization shortcuts remain deferred until
parity qualification.

## Complete and qualify a slice

Use current live Rust oracles, fresh case-bound guest completion, explicit exit
status and exact complete outputs. Timeouts, crashes, partial captures and launcher
success are failures. Keep positive and negative controls; distinguish rejection
from exact diagnostic parity. Focused tests, package generation and historical
corpus snapshots do not establish whole-language or family parity.

Before each coherent production change, identify unchanged input/settings and
record release-time, executable and linked-storage impact. Use separate
instrumented runs for phase/heap evidence; static storage is not peak RAM. Keep
cache in `target` only during an active batch, preserve needed diagnostics outside
it, and coordinate `make clean` after active builds/native runs finish.

Claim opcore complete only after source acquisition, macros/statements,
expressions, data/layout, modules/parameters, CLI/output, diagnostics and VM
contract gaps are qualified against current Rust behavior. Rerun complete
unchanged corpus roots and fresh full current-source self-host at that boundary,
and review combined responsibility boundaries. Then qualify each CPU family in
the stated order, including registered pipelines and current examples. Promotion
from experimental needs the full language/target/CLI matrix, representative
multi-module projects, useful diagnostics and honest resource requirements.
Qualify the 68020/AmigaOS floor and 2 MiB product goal separately from the 74 MiB
emulator result. Commit coherent recovery points; pushes require explicit authorization.

## Maintained documentation owners

- [Compact frontend boundary](compact-frontend-vm-boundary.md): implemented subsets,
  explicit limits and identified self-host proof.
- [Native source execution](native-source-execution.md): pending source-session design.
- [Native VM consolidation](native-vm-consolidation.md): pending contract migration.
- [Package execution boundaries](package-execution-boundaries.md): shared directive
  and package responsibility boundaries.
- [Native guide](../../native/README.md) and [reference manual](../opForge-reference-manual.md):
  invocation and user behavior.
- [VM efficiency](../performance/vm-efficiency.md): measurement procedure and evidence.
- [Native parity contract](../../agents/rules/native-rust-parity-porting.md): fresh-run proof.

This page owns current status and completion order. Git history retains superseded
checkpoint chronology, experiments and measurements; no historical checkpoint
anchors or archive tree are maintained here.
