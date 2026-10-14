# Relocatable fragments, lazy preparation and module reuse

Status: first fragment experiment qualified on 2026-10-14. Header compaction `8ba07c03` remains the reference. The default
execution path is retained; only `OPFORGE_FRAGMENT_SOURCE` selects the experiment.

## Outcome and sequence

Reduce cold-build preparation time and transient memory first, then broaden package
residency control and add repeat-build reuse. Preserve source behavior, whole-block
reachability, diagnostics/provenance, binary expansion and package-owned semantics.

| Order | Reviewable scope | Dependency and evidence |
|---|---|---|
| 1 | Shared relocatable fragment model, initially in memory | Reuse existing packed records, block ownership, expression/encoding recipes and relocation machinery. Establish identity/context/lifetime boundaries before changing execution scheduling. |
| 2 | Lazy preparation and earlier capture retirement | Request required definitions and active/reachable bodies; retain complete referenced blocks and binary templates. Measure actual avoided preparation and ownership on unchanged inputs. |
| 3 | Configuration-driven package acquisition | Active `.cpu`, `.fpu`, `.apollo` and other explicit configuration choices trigger loading. Package metadata declares shared-chunk dependencies; instruction use never implicitly loads capabilities. |
| 4 | Persistent `.ofm` objects | Serialize reusable fragments, indexes and recipes after their execution dependencies are established. Add semantic fingerprints, resolved dependency metadata and a bounded cache lifecycle. |

The fragment representation and disk cache are separate increments. Package chunk
loading is not a hard prerequisite for caching: opaque package references can first
resolve against whole loaded images. It is ordered earlier to qualify source-context
activation and full-target residency before persisting those dependencies.

Current [performance evidence](native-performance-diagnosis.md#compact-retained-capture-headers)
provides the baseline: 46,997,384 tracked peak heap bytes, 31,862,784 retained capture
capacity and 2,960,640 live bytes at assembly entry. The selected package is
454,890 bytes, with a 432,626-byte runtime prefix. In the separate instrumented
68040 run, preparation takes 69.52 s and assembly 34.54 s; binding/raw records
36.26 s, expression preparation 7.13 s and tokenization 6.79 s are preparation
subcomponents, not additional elapsed time. These measurements motivate the order;
they do not predict lazy-preparation savings or describe total process RAM.

## Shared representation requirements

Every fragment is relocatable, including conditional alternatives, loop bodies and
ready byte payloads. Addresses use fragment/section identities and local offsets.
Stored source, plans, templates, dependencies and object references contain no
process pointers; execution can borrow bounded views from identified owners.

Keep these distinct:

- Moving a backing allocation requires resolving owner-relative views again.
- Moving emitted bytes requires symbol/section relocation and encoding constraints.
- Reusing a configured module requires remapping object-local symbols/imports and
  restoring the correct package, lexical and expansion context.

A fragment may have fixed-size prepared bytes or a deferred recipe whose size or
activation depends on evaluation. Keep one body plus a recipe for loops, and a tree
of alternatives for unresolved conditionals; do not enumerate iteration counts or
all combinations of nested branches. Fold only decisions justified by invariant
values recorded in the configured artifact's dependencies.

Reverse indexes associate symbols/value occurrences with dependent sites: fixed
patches, expressions, guards, loop inputs, variable-size encoding recipes and block
references. Include placement-dependent and mutable values, not only symbols that
happened to remain unresolved at cache creation. Site context must preserve ordered
writes, snapshots and invocation/iteration identity. A reverse index schedules
existing evaluation; it does not replace its semantics.

Inactive alternatives contribute no emitted bytes, block roots, file acquisition
or package activation. Required structural/declaration checks retain their existing
policy. Constants, schemas and templates can be semantically required without
emitting bytes. Referencing an entry, preceding entry label or internal label retains
the whole owning `.block`, including fallthrough. Macros/segments remain templates,
not independently pruned code blocks.

Keep evaluation, directive handling, instruction selection and relaxation in their
existing shared VM/package owners. The envelope owns ranges, identities, dependencies
and lifetime, not another parser, evaluator or encoder. Preserve bounded convergence,
strict final guards, source-order effects and qualified rejection boundaries.

Persistent `.ofm` later contains symbol/use directories, relocatable byte fragments,
compiled expressions, control/expansion recipes and required provenance. Fingerprints
cover typed configuration, relevant options, selected packages, source/dependency
content and resolution choices. Timestamps are hints. Partial caches are valid:
preparing every inactive body just to populate a cache defeats laziness. Persistent
reuse needs an explicitly agreed size/expiry policy because the current workflow
empties `target` after each batch. No legacy VM executor or native VM-version dispatch
is introduced; container integrity and bounds validation remain required.

## First investigation: current-code mapping

This was source/fixture inspection and reuse of existing measurements only: no code
changes, compilation, emulator execution, cache generation or new performance claim.

| Boundary | Existing owner and evidence | Missing contract |
|---|---|---|
| Tokenized acquisition | [capture](../../native/motorola68000/amigaos/experimental/binary_capture.asm), `Record/create/resolve`, and [frontend](../../native/motorola68000/amigaos/experimental/binary_frontend.asm), `captureLine` | Capture has a 56-byte header, 20-byte TKVM rows, lexemes and candidate plans. It is an unbound acquisition record, not compact final byte output. Candidate plans are already acquired before reachability. |
| Binary prepared source | [source](../../native/motorola68000/amigaos/experimental/binary_source.asm), `RecordHeader/NameToken/PlanToken` | Records have byte lengths, numeric names and compiled/typed payloads. Session IDs and plan ownership need an object-local identity/context map before persistent reuse. |
| Dependency-first preparation | [schedule](../../native/motorola68000/amigaos/experimental/binary_source_schedule.i), `capturePhysicalLine/orderCapturedSources/replayCapturedSources`, and [app](../../native/motorola68000/amigaos/experimental/binary_app.asm), `prepare` | Capture regions survive ordered replay. Preparation and ordered-copy work precede `indexBlocks/selectBlocks`; omission of code does not prove its preparation was avoided. |
| Whole blocks/references | [block index](../../native/motorola68000/amigaos/experimental/binary_block_index.asm), `Span/index/select/references/mark` | Start/end/entry/parent/live fields and numeric reference scans already retain whole outer blocks. Dead spans are flagged in place. There is no independently owned fragment or symbol-to-dependent-site index. |
| Ordered origins | [ordered records](../../native/motorola68000/amigaos/experimental/binary_ordered_records.asm), `materialize` | Copies module spans and translates physical-origin/module boundaries. Fragment-local origin and record mappings must preserve this provenance. |
| Sizing and execution | [assembly](../../native/motorola68000/amigaos/experimental/binary_assembly.asm), `beginSizing/execute/finishSizing`, and [session](../../native/motorola68000/amigaos/experimental/binary_assembly_session.asm), `State` | Records, frontier, symbol count/cell buffers/member keys and module boundaries are frozen. Mid-session relocation or publication explicitly rejects. A fragment view must initially honor that boundary. |
| Conditional observations | [branches](../../native/motorola68000/amigaos/experimental/binary_branches.asm), `evaluate/evaluateScalar`, and [trace](../../native/motorola68000/amigaos/experimental/binary_branch_trace.asm), `observe` | Existing expression evaluation permits only qualified provisional guards; final unavailable guards reject. Observations identify source offsets in traversal order, with invocation identity supplied by repetition. Stable logical site mappings must preserve that identity. |
| Repetition/declarations | [repetition](../../native/motorola68000/amigaos/experimental/binary_repetition.asm), `Slot/refresh`, [inventory](../../native/motorola68000/amigaos/experimental/binary_repeat_inventory.asm), `prepare/refresh`, and [snapshots](../../native/motorola68000/amigaos/experimental/binary_repeat_records.asm), `capture` | Body/header and typed payloads have offsets; inventory refresh still requires a sealed symbol count. This supports later ownership work, not arbitrary append or lazy publication today. |
| Output relocation | [assembly](../../native/motorola68000/amigaos/experimental/binary_assembly.asm), `OutputReloc/markInstructionRelocs/markDataReloc`, [Hunk references](../../native/motorola68000/amigaos/experimental/binary_hunk_references.asm), `affineTarget`, and [encoding](../../native/motorola68000/amigaos/experimental/binary_encoding.asm), `outputFixup` | Existing output fixups are section-based and bounded absolute longwords. They are not the complete symbol-use/guard/loop index proposed for `.ofm`; retain more general recipes before final format-specific fixups. |

### Representative module and necessity/lifetime trace

Inspect [binary_capture.asm](../../native/motorola68000/amigaos/experimental/binary_capture.asm)
as a bounded real module. Its declarations define Frame/Record fields and constants;
public `create`, `resolve` and `planView` bodies use private `validate`, `copy` and
`clearPadding` blocks. `create` also references the imported `memory.reserve` block.
The includes provide gated telemetry templates. A used public body needs complete
helper/import closure; struct constants and template definitions can be needed even
where they emit nothing. Disabled telemetry expansion must remain zero emission.

The trace is: physical-line tokenization/candidate capture -> configuration/index
scan -> dependency-ordered binary replay and binding/compilation -> prepared record
materialization -> block index/reference selection -> sizing and section emission.
Capture records survive acquisition and ordered replay. Successful
`replayCapturedSources` immediately calls `releaseCapturedSources` before returning;
failure cleanup releases remaining capture ownership. Prepared numeric records and
exported values survive assembly. Earlier retirement must therefore occur within
replay or avoid unnecessary acquisition/preparation; it is not a missing final
cleanup. This source inspection identifies ownership boundaries but does not assign
bytes or elapsed seconds to this module, or establish the ratio of unused to useful
preparation.

Control examples are inspected in the existing [forward branches](../../crates/opforge-asm/src/tests/native_forward_branches.rs)
(`inactive-invalid-values`, `guarded-count-alternative-false`), [iterable loops](../../crates/opforge-asm/src/tests/native_iterable_loops.rs)
(iterator shadowing/restoration), [module graph](../../crates/opforge-asm/src/tests/binary_source_graph.rs)
(internal/preceding labels and transitive block ownership), and [Hunk data relocations](../../crates/opforge-asm/src/tests/binary_source_hunk_data_relocations.rs)
(imported long plus addend). Older graph controls contain explicit unqualified
unsectioned cases; use a sectioned live Rust oracle for the eventual experiment.
These fixtures locate semantics and future comparisons; they were not rerun here.

## First implementation experiment

Wrap one already prepared selected outer block in an in-memory descriptor with
identity, backing-owner/range, entry/local declaration mappings, section association
and origin mapping. Retain the complete surrounding stream, existing sealed symbol
inventory and frozen assembly driver. Record a small dependent-site table for a
fixed-width long reference and a forward guard using existing numeric expression
and reference readers. This table describes uses; it does not schedule selective
replay yet. No `.ofm` serializer, new grammar, dynamic loading or lazy body preparation.

Use a small sectioned root/dependency fixture with a referenced whole block,
an internal fallthrough label, a transitive helper, an unused sibling block, one
cross-section long relocation, a forward conditional and a bounded loop body.
Exercise both conditional outcomes and two legal placements. Compare logical
references, selected blocks, complete bytes/labels/fixups and guard/loop observations
with the current path and live Rust. Initial descriptor offsets resolve into the
unchanged prepared stream; they do not make that block independently executable.

Copy the complete backing records before `beginSizing`, reconstruct all dependent
views, and repeat the comparison. Preserve the existing rejection of movement or
growth after session start. Validate malformed descriptor extents and failed setup
without partial publication. This first step proves representation and rebinding
at the existing lifecycle boundary, not autonomous fragment linking or incremental
source execution. Keep new envelope ownership outside the large frontend/app.

Success: stable logical identities, bounded owner-relative references, unchanged
whole-block selection and exact output/observations across both preparations.
Report descriptor/static/heap cost and separate unchanged-input release timings;
no speed or memory saving is required or claimed for this foundation. Stop to
reconsider if the bounded proof requires a new evaluator, general layout rewrite,
weakened frozen-session checks or broad parity expansion. Subsequent lazy work
needs an explicit inventory/publication/lifetime design and actual skipped-work
measurements, rather than treating this wrapper as lazy execution.

Use focused native controls first. At a coherent production boundary preserve the
header baseline, compare identical inputs/settings, measure release and memory
separately and perform a fresh full current-source self-host. A6000 is currently
unavailable; FS-UAE profiles and evidence limits remain explicit. The [source-session
plan](native-source-execution.md) owns growing frontiers and actual-state acquisition;
this plan supplies the fragment foundation and revised order without activating
unimplemented scope in historical plans.

## Current implementation boundaries

`binary_fragment` owns a 20-byte fragment descriptor and a 104-byte temporary
owner state. It selects the first retained outer block from the existing block
index and records 20-byte rows for its complete record range: local offset/extent,
numeric declaration, line, module ordinal and section-control offset. Map storage
is based on the actual number of records. The shared `binary_source.RECORD_LIMIT`
keeps the reference path's 2 MiB prepared-stream limit.

The experiment copies the **entire prepared source allocation**, resolves the
selected fragment against that new base, then invokes the existing assembly
driver. Borrowed symbols and module boundaries remain sealed and unchanged. The
original record pointer is restored and all candidate allocations are retired on
success or failure. Empty streams use the reference driver directly. Relocation
after `beginSizing` remains forbidden. No stored view contains a process pointer.

`binary_fragment_sites` observes the first eligible long-data expression and
IF/ELSEIF guard using the existing PRVM services. Ordinary `.long` discovery
reuses the validated numeric-head selector layout in a 36-byte stack copy with
the package's long-directive ID; `.emit long` retains the packed-data service.
Neither route executes a directive or evaluates an expression. It records
full-backing record and operand offsets, capsule extent and a numeric symbol ID for exact symbol-leaf
expressions; other expressions remain opaque recipes. These two rows are an
initial observation table, not a complete reverse dependency index or scheduling
mechanism. The guard role does not establish whether a particular value is
forward or already known. Session symbol IDs are retained; persistent object-local
remapping is still future work.

The focused control includes both source-selected BIN and relocation-bearing Hunk
outputs. CLI `--bin` plus source-selected Hunk is rejected by the reference CLI,
so this control uses two regular `.output` descriptors. Both conditional branches
and two placements have live Rust oracles. A disposable test-only build checks
register/CCR behavior, descriptor bounds, record/declaration mapping, retirement,
stable identity and active-session rejection. Production behavior never depends
on the control's identity.

Release timings compare the same 1,200 macro-call workload. Separate MEMD
runs compare identical full-source inputs. Map/index allocations retain the
allocator's ordinary 1 MiB scratch bound;
this is a bounded envelope experiment, not a general large-object implementation.
The envelope intentionally adds a complete copy; savings require the
later lazy-preparation step. Full enabled self-host uses a frozen source snapshot
with the two build-selection `.ifdef` guards materialized as `.if 1`, because the
current native CLI does not inject build defines. Rust and native assemble the
exact same selected source. This is distinct from a candidate assembling the
default reference source.

The counter `Prepared` and a local `prepared` label originally collided under
case-insensitive lexical lookup. The local label is now `ready`. Instrumented
repeated preparation and the serialized setup/copy/map counters are checked;
release bytes are unchanged by this repair. Earlier accounting captures with a
zero setup count are superseded, not accepted as final measurement evidence.

## First experiment results

Keep this as an opt-in foundation. The reference path remains the default. No
speed or memory saving is claimed; independent fragment execution, lazy
preparation, a complete dependency index and `.ofm` persistence remain unimplemented.

The final reference/candidate/probe release builds pass all twelve fresh native
controls: both guard outcomes and both placements, with exact BIN and complete
Hunk output. The corrected instrumented probe also passes all four cases,
including repeated setup, failed setup and frozen-session rejection. Host oracle
checks, Clippy with warnings denied, workflow/architecture checks, native formatting
and the focused redundant-test check pass. The repository-wide redundant-test
scan still reports 17 existing autofixable findings outside the changed work;
this experiment does not claim that broad check passes.

### Release timing: bounded mixed control

Same 11,439-byte source, 1,200 macro calls, 7,224-byte BIN and exact Hunk oracle.
FS-UAE uses `68020-10m`: 68020, 2 MiB chip plus 8 MiB fast memory. Both bootstraps
embed the same 68020 and 6502 packages. These are uninstrumented host observations
between fresh guest START and DONE, including acquisition and output writing.

| Measurement | Reference | Fragment | Difference |
|---|---:|---:|---:|
| Release elapsed | 128.4001 s | 128.6525 s | +0.2524 s (+0.20%) |
| Hunk file | 747,500 B | 749,128 B | +1,628 B |
| Linked static reservation | 781,272 B | 782,932 B | +1,660 B |

This single paired observation shows no meaningful timing change; it cannot
establish a precise regression or speedup. Input digest is
`fnv1a64:1fd0f877d15b3ecb`. Receipts and source are retained under
`/tmp/opforge-fragment-control-evidence-20261013`.

### Memory: identical full-source input

The separate accounting pair assembles the same 198 files / 2,614,471 bytes,
including the generated catalog and embedded-package inputs. Both bootstraps
embed 68020 and 6502; source output embeds only 68020. Both produce the complete
708,592-byte live Rust Hunk with fresh completion and exit zero. The expanded
`existing` FS-UAE profile is A4000/68040, 2 MiB chip, 8 MiB fast, 64 MiB Zorro III
and 16 MiB graphics memory. Instrumented times are diagnostic, not release timing.

| Tracked allocation measurement | Reference | Fragment | Difference |
|---|---:|---:|---:|
| Peak owned heap | 47,587,208 B | 47,587,208 B | 0 |
| Total allocated / freed | 112,389,800 B each | 114,512,464 B each | +2,122,664 B each |
| Prepared record bytes | 2,043,148 B | 2,043,148 B | 0 |
| Terminal owned bytes | 0 | 0 | 0 |

The added ownership is a 2,097,152-byte complete-backing allocation, a
22,648-byte block index and 2,864-byte mapping allocation (143 mapped records).
It is retired before return; earlier preparation already determines the measured
peak. The setup/copy/map report confirms one setup and 2,043,148 actual bytes
copied. There are no profiling errors or allocation failures. These figures
exclude executable/static storage, stack and OS allocations, and do **not**
qualify the 2 MiB product budget.

Final input SHA-256 is
`904574c5687292a19f09f8c881563d510e61275f8c821c23a364ff284066deb7`;
reference/candidate accounting image digests are
`fnv1a64:6832c8e0cd06e9ab` / `fnv1a64:9d8f1953118bb669`. Raw captures and decoded
accounting are retained in `/tmp/opforge-fragment-v11-full-reference-memory-20261014`,
`/tmp/opforge-fragment-v11-full-candidate-memory-20261014` and
`/tmp/opforge-fragment-final-memory-20261014.json`.

### Full enabled native self-host

The release bootstrap rebuilt its own selected source configuration in
**103.1142 seconds**, exit zero, with an exact **708,592-byte** live Rust match
(`fnv1a64:b484aa75145aa590`). Both bootstrap and output embed only 68020. The
materialized build-selection snapshot's source manifest is
`fnv1a64:22e86fe770bff9b6`; the same expanded 68040 profile above was used. This
is an actual full enabled self-host, separate from the small control and from
instrumented fixed-input comparisons. It does not qualify all language/family
features, hardware execution or constrained-memory operation.

The qualified source/export lives at
`/tmp/opforge-fragment-v11-selfhost-release-20261014`; full case and captures are
at `/tmp/opforge-fragment-v11-full-20261014`. The local FS-UAE result's bootstrap
digest is now captured before execution: source-directed output can overwrite
`build/opforge_compact`, so hashing that pathname after the run incorrectly
reported the output as the bootstrap. The full comparison checks the actual
input image identity explicitly.

The [lazy-preparation design](native-lazy-preparation.md) now defines the
inventory/publication/lifetime boundary and the next shadow-audit checkpoint.
That checkpoint is not implemented yet. This result qualifies movement of a
frozen prepared stream and its bounded envelope; it does not remove the preparation
work responsible for the large capture peak.
