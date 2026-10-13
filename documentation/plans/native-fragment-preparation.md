# Relocatable fragments, lazy preparation and module reuse

Status: revised direction agreed on 2026-10-13; first read-only investigation
completed. Implementation is not authorized by this checkpoint. Header compaction
`8ba07c03` and its qualified images remain the reference implementation.

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

## Proposed first implementation experiment, awaiting authorization

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
