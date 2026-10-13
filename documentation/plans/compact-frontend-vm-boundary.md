# Compact frontend: VM boundary correction

Status: source commit `7a903cf8` has complete embedded release native self-host
qualification on 2026-10-13, with exact complete live Rust Hunk equality.
The same release bundle also passes on the physical A6000 in 118 seconds.
The separate instrumented A6000 run measures 46,880,904 tracked peak bytes;
the 2 MiB target and complete opcore/CPU-family parity remain open.

Rust has one consolidated EXVM v1 grammar/executor. Native preparation executes
it over bound numeric tokens and lowers a transient offset-based tree to ExprVM
programs; native EXVM has no version argument, field, check or dispatch. Known
source-site scalar, flat-list and range `.use ... with (...)` values, mutable
snapshots, compound forwarding and packed iterable `.for` have focused coverage.
Imported iterator identities, deferred macro register/value binding and lexical
dotted-name lookup have focused native coverage.

## Package members and shared value members

`binary_member_syntax` selects acquisition-only facets from the package’s numeric
`MemberBinding` rows: instruction, qualifier, operand position and field identity.
Literal and parenthesized scalar bases retain their package member wrapper. EXVM
treats the selected facet as an enclosing delimiter and validates the scalar
base; preparation strips the transient role before persisting numeric operands.
Ordinary struct members keep shared typed expression evaluation. This does not
introduce field spellings or CPU-specific suffix rules into the shared evaluator.

Focused native controls cover absolute widths, disabled telemetry neighbors and
ordinary struct fields. The full-extension tuple width variation remains an
existing gap in both pre-repair and candidate images. See the
[current completion plan](native-runtime-reset.md) for full self-host evidence
and the separate performance/memory comparison.

## Repetition and conditional execution

Packed replay uses package-owned numeric BranchPlan/LoopPlan roles without
parsing source text. Rust skips expression evaluation under inactive parents and
after an earlier branch wins, while checking syntax and branch structure.
Iterator-dependent conditionals, shadowing/restoration and register/value binding
have focused live Rust comparisons on 68020 and 6502.

Anonymous and labeled counted/list/range `.bfor`, and anonymous/labeled `.bwhile`,
have focused coverage for fresh local scopes, nested composition and retained
iteration records. Scoped `.bfor` admits deferred value declarations; deferred
scopes, configuration, metadata, file operations and macro definitions still
reject. Ordinary loops retain their declaration restrictions. Complete lazy
file/reachability parity and mapped scoped replay remain open.

Local immutable roots activate when shared PRVM replay selects their branch.
Unconditional forward constants retain eager dependency traversal; inactive
candidates do not evaluate. Bounded sizing replay uses retained iteration cells
and shared EXVM evaluation for conditions against later local mutable values;
final emission checks strict guard and loop traces. New loop paths and guards
without an executed initializer reject. Iterable snapshots bind each element
before resolving local constants and restore iterator bindings at close.
General forward-alias/layout convergence remains outside this coverage.

Rust relocation classification follows active iterator lookup. Direct iterator
data and captured scalar constants do not inherit a shadowed section label's
fixup; qualified original addresses still relocate.

## Current opcore breadth and acquisition boundary

The fresh correctness-only 86-root opcore audit completes every guest protocol:
27 exact positive outputs, 36 negative rejection controls, and 23 positive roots
rejected with exit 20. There are no timeouts. Negative rejection does not qualify
exact diagnostic parity. The current [completion plan](native-runtime-reset.md)
records the ordered work and evidence limits; this audit supersedes the earlier
selected opcore corpus observation, without updating CPU-family qualification.

The source cursor traverses physical records or the currently expanded numeric
template body. Previews reuse normal substitution, restore invocation state and
use a separate output buffer. Loop records retain their owning stream identity;
a nested expansion cannot close its parent's loop. Shared source control detects
file actions reached through known template calls. Ordinary forward-count calls
without acquisition retain numeric runtime execution; anonymous scoped iterations
keep their lexical owner. Rust relayout preserves module template visibility and
captured definitions needed by active includes while rebuilding emitted state.

Preview borrows the source session and suppresses reference/register publication,
restoring pending binding state. Scratch identity interning remains monotonic.
TKVM's invalid-composed annotation and PRVM positional spans select whole-line
substitution before ordinary operand lowering. The package-selected numeric head
binds immediately; generated operands bind after shared fragment tokenization.
Inactive generated records intern identities without publishing semantic references
or retaining imported blocks; borrowed binding mode restores on success and failure.

Nine active-acquisition behaviors on each of 68020 and 6502 pass after a fresh
retry of one pre-START stall (18 positives total). Missing active files, include
cycles, unavailable forward guards and acquired/expanded module graph changes
have seven fresh rejection controls. Inactive composed qualified members now pass
both packages, and malformed ordinary `@0` rejects. Direct and composed imported
typed-member controls now pass through numeric proxy owner indexing: finalized
plans retain possible unknown/mutable prefixes and stop at a known immutable
compound. A common possible block owner is indexed once; differing owners reject
only when reached. Eighteen positive controls and two rejection controls pass
freshly across both packages, including forward fields and mutable-kind changes.
Forward compound aliases and dynamic selection between different physical owners
remain unqualified. Named acquired collections pass twelve fresh exact controls, including empty,
counted/iterable/while snapshots, nesting, definition-relative macro/segment
includes and colonless/explicit-colon labels. Private numeric boundaries publish
complete bounded batches; inventory and repetition owners independently validate
label/index/nesting/balance and accumulate actual assembly address records without
rerunning already materialized bodies. Static included-source capture now passes
18 exact cases and 12 expected rejections across both packages and unchanged
default-8085 examples. Capture acquires and indexes static child records before
ordering modules, retaining physical origins and using the shared FilePlan
selector. Actual module search, included `.use` edges, selected-file sibling
validation and include cycles are covered. Runtime/expanded graph changes and
PC-dependent acquisition remain unqualified; the
[source-session design](native-source-execution.md) addresses the pending structural
work. These focused results do not establish complete opcore or a new self-host.

## Full current-source self-host qualification

The subsequent opaque-asset repair `7a903cf8` has fresh focused native proof for
the 4,099-byte patterned asset, the generated full catalog containing the actual
454,890-byte package, text encoding isolation, and malformed payload/remapping
controls. The catalog emits a complete 456,992-byte Hunk matching its live Rust
oracle. These focused results do not establish the complete embedded self-build.

Its separate unchanged 11,592-byte control on FS-UAE 68020/10 MiB takes
41.802194583 seconds before and 41.802430625 after in release mode: no measurable
change in this single pair. Release image size grows 747,132 to 747,424 bytes,
and linked reservation grows 780,928 to 781,196 bytes. Separate instrumented
controls retain the same 1,240,712-byte peak, 27,022-byte packed source,
3,145,864 allocated/freed bytes and zero live ownership. This control contains
no binary asset, so it measures repair overhead, not asset-path speedup.
Report: `/tmp/opforge-binary-assets-cost-20261013.json`.

The complete configured release self-build of `7a903cf8` also passes on FS-UAE
68020/74 MiB: all 194 mapped inputs (2,580,296 bytes including the package)
assemble with fresh START/DONE, explicit exit zero and exact complete live Rust
Hunk equality. Both bootstrap and assembled output embed only the unchanged
454,890-byte m68020 package. Output is 706,888 bytes, linked reservation is
740,660 bytes, and SHA-256 is
`5d5e6f10c15dacf4b8b3840119380032755551d51eafe897bcb668985f4f19ee`.
Source manifest: `fnv1a64:ec940cb60e2202f6`. Release START-to-DONE is
3,512.267856959 seconds (58m32.27s). This is a complete embedded self-build,
not an isolated catalog test. Memory instrumentation was disabled, so no new
full-workload allocation claim is made. Capture:
`/tmp/opforge-binary-assets-full-capture-20261013/`; exported bundle:
`/tmp/opforge-a6000-7a903cf8-release`.

The earlier unembedded source commit `eed674a3` self-assembles the 193-file source set
(2,122,797 bytes), manifest `fnv1a64:befc1361cc9722cd`, on FS-UAE 68020/74 MiB.
Fresh case-bound START/DONE and native exit zero accompany exact live Rust equality
for the entire 251,704-byte Hunk, with 285,500 bytes linked reservation and SHA-256
`8bc6a2e0474a98d55d212a9d393276a6734db9ce148efe1c6ccc3a7c5b798083`.
The prepared m68020 runtime package is 454,890 bytes. This is the complete source
assembly, not a reduced probe or partial artifact comparison.

The phase/memory-instrumented bootstrap takes 3,568.574592333 seconds from fresh
START to DONE (59m28.57s). Separate guest phase measurements report 2,329.72 seconds
preparation and 1,241.58 seconds assembly. Tracked peak allocation is 47,319,416
bytes; all 108,612,240 allocated bytes are freed, with zero live ownership and no
allocation failure. This is instrumented emulator evidence, not release timing,
physical A6000 execution or 2 MiB feasibility. Earlier BS35 release timing uses
different source and package inputs and is not a before/after comparison.

The proof uses `compact_cli_self_host_entry_readiness_fs_uae` with
`OPFORGE_COMPARE_MEMORY=1`, `OPFORGE_PREPARATION_PROGRESS=1`,
`OPFORGE_PHASE_ONLY=1`, the configured `68020-74m` profile and generous fresh-run
timeouts. Its Rust oracle is assembled from the current filesystem; the full
output matches in the same guest run. Receipt and captured output:
`/tmp/opforge-selfhost-member-capture-20261013/`.
This qualifies that source checkpoint only. Its instrumented time and different
output are not a before/after timing comparison for the embedded release run.

A separate embedded bundle can be qualified using `export_compact_self_host_bundle` with
`OPFORGE_COMPACT_EXPORT_OUTPUT_EMBED=68020`,
`OPFORGE_COMPACT_EXPORT_NATIVE=1`, `OPFORGE_FS_UAE_TIMEOUT_MS=10800000`
and `OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=10200000`, a new absolute
`OPFORGE_COMPACT_EXPORT_DIR` and the [configured FS-UAE environment](../../agents/rules/fs-uae.md).
Host export alone does not establish native completion. Fresh case-bound
START/DONE, explicit exit zero and exact complete Hunk equality are required;
stored manifests or outputs cannot replace the live oracle.

## Physical A6000 execution

The current emulator-qualified release bundle is
`/tmp/opforge-a6000-7a903cf8-release`, selected by `/tmp/opforge-a6000-current`.
Bootstrap and assembled output both embed only `m68020--motorola68k.bin`;
there is no external runtime fallback. It contains 194 mapped inputs
(2,580,296 bytes including the package asset) and a 706,888-byte executable,
with 740,660 bytes linked reservation. Local and relocated Rust assembly match,
and the A6000 runner's dry run validates the actual bundle. Its manifest records
`native_validation: fresh_fs_uae_exact_hunk`. A fresh physical A6000 run of the
same bundle completes with exit zero and exact complete Hunk equality in
118 seconds (one-second guest clock resolution), with 118.3560407909099 seconds
of host command time including connection. This is release mode, with no
instrumentation defines. Its case digest
`c469c0fb6c9894cf8948514016b02e2fdee467095dd94f1ddc344b1db42d6d0b`
matches the current bundle. The captured output independently matches its Rust
oracle and the emulator output SHA-256 above. Receipt:
`/var/folders/98/663my3_95hqdv6wyjc99vkrw0000gn/T/opforge-a6000-result-ewffglzb/result.json`.

The separate instrumented bootstrap runs the identical release source/package/
command/oracle on the A6000, with fresh exit zero and exact complete output.
It takes 191 seconds including instrumentation: preparation 155.38 seconds and
assembly 35.14 seconds. Tracked peak is 46,880,904 bytes; all 109,772,320 allocated
bytes are freed, with zero live ownership, no allocation failures and no profiling
errors. The instrumented hardware case is
`3538c2ea566915b2b5d01c5a6d0f1e1e36cb164219815bcdb8314f0383154baa`.
Its initial host-only candidate status describes the state before this fresh
hardware qualification. Receipt:
`/var/folders/98/663my3_95hqdv6wyjc99vkrw0000gn/T/opforge-a6000-result-4i2o6ro3/result.json`.
The redundant instrumented emulator attempt was cancelled after hardware proof;
there is no completed instrumented emulator result for this embedded configuration.
See [resource diagnosis](native-performance-diagnosis.md) for attribution,
instrumentation overhead and optimization candidates.

The preceding bundle failed a fresh A6000 run with exit 20 after 101 seconds at
`catalog.i` line 572, the package `.incbin`; no output was produced. Binary assets
had been stored as ordinary strings and passed through text encoding. The current
bundle includes the opaque-byte repair and complete embedded emulator proof above.

The 31-byte `binary_declaration_activity.asm` filename was shortened to
`binary_decl_activity.asm` without changing module identity or source bytes.
The resulting unembedded Rust Hunk is byte-identical to the completed native
self-host output. The runner now validates the current 116-byte declaration plan,
retaining rejection of the superseded 23-byte size. All 26 runner tests pass.
Run from macOS Terminal, where `ash` and `acp` can reach the A6000:

```sh
python3 /Users/erik/Code/Retro/opForge/scripts/performance/run_a6000_selfhost.py
```

Defaults are host `192.168.0.220`, volume `Development` and a one-hour assembly
timeout. Each invocation creates its own remote directory and local result tree.
The current script validates BS38 package headers, mapped source preamble and
exact source/package/image bytes before transfer, then round-trips all inputs
before execution. Filename components must fit the 30-byte classic limit. A
fresh matching BS38 bundle is required for the current helper.
Guest `Date` brackets assembly at one-second resolution, excluding transfer;
host command time includes connection and Shell setup. Success requires fresh
case-bound markers, explicit guest exit zero and exact full-Hunk output. A timeout
can leave a guest command running: inspect that run before retrying.

The runner prints the selected bundle's native qualification before transfer and
records it as `bundle_prior_native_validation` in the result. Host-only exports,
failed diagnostic captures and exact qualified bundles have distinct statuses;
inconsistent success claims reject. That prior status describes this exact bundle,
including embedded/external package configuration. The new hardware run's own
completion, exit and artifact comparison determine its success independently.

Instrumented configurations must be regenerated for the current package format
and qualified before hardware selection. Use separate release and instrumented
runs against identical source/package cases. `OPFORGE_PHASE_ONLY=1` excludes
detailed per-opcode tokenizer probes; retained stdout/stderr and `memory.bin`
allow counter decoding after ephemeral runner cleanup. Balanced ownership, zero
profiling/allocation errors and valid clocks are required for telemetry validity.
Do not add nested binding/input observations to exclusive stage totals. Historical
physical timings and peak-memory captures describe different inputs and packages;
they do not qualify current memory needs or 2 MiB fit.

## Ownership and representation contracts

The compact path is a package-directed execution path. Rust and the canonical package
define semantic behavior; native code executes bounded shared primitives and
package-selected operations. Update package producers and consumers together and retain
only the latest supported format; version identifiers detect mismatches and do not
require compatibility executors.

| Responsibility | Contract |
| --- | --- |
| TKVM | Own line tokenization and package-selected lexical policy, including numeric normalization and explicit composed-name/substitution recipes. |
| PRVM | Own statement, operand, macro and other preparation grammar. Native code may validate bounds or execute selected primitives, but must not invent source grammar from text or packed offsets. |
| EXVM | Own expression parsing and evaluation for covered expressions. Native code may write or execute the selected expression program; a native precedence parser is not proof of VM ownership. |
| Native writer / assembler | Consume numeric identities, structured values, bounded expression programs and offset-based records. Preserve source provenance for diagnostics. Keep immutable preparation separate from mutable pass/layout state. |

Bootstrap and macro expansion may have host-owned steps where the VM boundary protocol
allows them. The relevant contract is which component selects the grammar and owns its
behavior, not whether native routines contain branches.

Persist offsets and numeric IDs, never process pointers. Every stored offset has an
explicit base and validated bounds; moving a block must not require patching stored
records. Scratch preparation storage is released before assembly. Hunk fixups retain
complete symbol and section identity through stabilization and output; unsafe arithmetic
and unsupported provenance fail closed. Generic directives and CPU/family semantics
remain in their established shared or package-owned layers.

## Portable token and package details

Rust `PortableTokenKind::Number` carries optional normalized `u64` metadata.
Native TKVM keeps its 20-byte token record: the former reserved word carries
numeric status, and scratch offsets refer to spelling followed by the normalized
eight-byte value. Package opcode `0x13` selects normalization and ordered radix
rules. Invalid and overflowing number spellings remain deferred until a caller
requires a value. This preserves numeric-looking package names, ordinary
numbers and substitution text on the same line.

Strings already carry decoded bytes. During preparation, package-selected
composed-name, number and string policies produce bounded recipes and offset
handles. The writer copies or binds the selected result; it must not parse the
same spelling a second time. Generated calls re-tokenize VM-bound argument
fragments. The remaining decoded-string body-token fallback is called out as an
unfinished boundary below.

The compact capsule embeds TKVM, macro descriptor programs and the current shared
EXVM expression-parser program. The native adapter builds bounded scalar nodes
from numeric IDs and decoded literals, then lowers them to the compact evaluator
format. Compound values and dotted calls remain backend gaps. Version identity
belongs to producer tooling; native EXVM assumes its compiled contract and checks
program, token and arena bounds without selecting a version.
The active frontend session owns one reusable expression workspace. `begin` and
`activate` install its grammar and scratch; `finish` invalidates that configuration.
Compilation and lowering reuse it sequentially, with no per-expression allocation
or large grammar workspace on the Amiga Shell stack. Persisted nodes remain offsets;
workspace pointers belong only to the transient calling interface.
The macro-only initial PRVM plan runs after binding. A separate shared four-byte
PRVM prefix policy now runs before binding: BeginStatement,
ParseOptionalLeadingLabel, FinishLine, End. The VM owns column-one label and
adjacent-colon decisions. The native adapter presents two logical tokens and
maps the returned cursor to a physical token index, including composed-name
recipes; the writer uses that index to distinguish package heads from values.

BS38 is the current compact package format; the producer writes `BS38` and the
native package owner checks the matching magic. Only this latest runtime contract
is supported; packages must be regenerated. Its 262-byte header retains TextPlan
and TextPlanBytes at offsets 228 and 232, followed by the named DS directive ID
as a u16 at offset 236. It appends three preparation-only offset/extent pairs:
statement compilation at 238/242, matching at 246/250 and head selection at
254/258. Their bounded programs lie after `RuntimeBytes`; package validation
checks each program through its owning shared PRVM validator and checks head
identities against the package name table. They do not select CPU semantics or
introduce native VM version dispatch. CLI statement routing is still pending
until the component and template integration work is qualified.
The shared 34-byte TextPlan supplies
numeric text directive actions and encoding-name roles. Its 17-byte LoopPlan retains numeric
scoped-for/scoped-while identities to the current envelope. This is one compiled
PRVM contract, with no native VM-version dispatch or legacy loop executor.

The named `.ds` directive uses shared scalar preparation and assembly-owned byte
reservation. Flat leading/trailing gaps produce no payload; internal gaps preserve
Bin holes without emitting Hex/S-record data. Initialized sections retain padding;
BSS uses independent fragment placement with no initialized bytes. Fragment geometry
does not by itself select Hunk serialization. Fresh 68020/6502 reservation and
Bin/Hex/S-record controls, plus the existing Hunk BSS regression, match live Rust.
This is focused qualification; complete opcore and current-source self-host remain
pending. [Measurements](../performance/vm-efficiency.md#shared-byte-reservations-bs37)
record this slice's separate cost.
Candidate rows are 44 bytes: offsets
0–31 retain their previous fields, byte 32 carries the third-operand form nibble,
byte 33 the third tuple class (class + 1), and word 34 is reserved zero.
Word 36 holds independent operand-role count, word 38 is reserved zero, and long
40 is the package-relative offset of a flat 12-byte projection array. An absent
array has count and offset zero; identical complete arrays share offsets. Roles
come from canonical selectors before encoding transport drops inputs. Fixed
opcodes and unsupported encoders can therefore retain structural binding roles.
Unknown predicates retain no partial role proof. Shapes
11/12/13 carry direct and immediate three-operand forms; 14 represents
register/immediate, 15 register/register/direct and 16 register/register/register.
These are structural distinctions; package projections own classes and semantics.
Projection kind 27 selects a
wrapped register child from call argument 0/1 on operand 0–2, using the same
class/value-program/argument fields as kind 24; no callee or opcode spelling is
interpreted by native transport. Raw colon normalization remains unsupported.
Target flags at 130 request structural
wrapper preservation (bit 0) and nested preparation (bit 1) from canonical projections.
Tuple register/value/qualified projections use kinds 11/12/13. Descriptor
bytes 10/11 hold arity and item index: arity 2/3 is exact, while 0 allows only
actual arity 2/3 when the canonical plan has no explicit arity predicate.
Kind 24 selects argument 0/1 from a complete dotted numeric call, preserving
the package register class and standard value-program field; Literal is zero
and the final word stores the argument ordinal. It requires the selected argument
to exist, not an exact arity or callee spelling. Mask kind 18 supports one class
with an absent second-class sentinel `$ffff` and zero second shift.
Kind 25 requires an immediate (`#`) wrapper around the selected operand and
evaluates its scalar expression. Class, Literal and Reserved are zero; the
standard value-program field remains available. The package owns value bounds
and encoding; native code only validates the structural wrapper.
Kind 26 traverses package-owned numeric expression paths. Its Class word is
path byte length (4–32, a multiple of four), Literal is a package-base-relative
path offset and Reserved is zero. The standard value-program field remains
available. Deduplicated path programs occupy the arena between the 262-byte
header and candidate rows; projection descriptor arrays stay contiguous.
Each step is `opcode:u8, argument:u8, parameter:u16` in big-endian order:
indirect/bracket unwrap (1/2), tuple child 0–2 (3), register/class (4), qualified
register/class (5), scale (6), or member value/field ID (7). Container parameters
are zero; only the final step is a terminal. Unknown operations remain unsupported.
Packed preparation retains bounded delimiters, numeric names and opaque scalar/
product capsules; qualified displacement-prefix syntax normalizes to the same
structure as the equivalent tuple spelling. Ordinary lexical struct fields keep
the existing scalar path. Traversal distinguishes singleton wrapper interiors
from tuples, reads no source strings and persists no memory pointers.
Qualified products use full 64-bit right-first scalar evaluation; scale accepts
only the first successful scalar value 1/2/4/8. Classes, qualifiers, member IDs,
ranges and emission semantics remain canonical package data. The implementation
lives in `binary_nested_operands.asm` and `binary_operand_paths.asm`; existing
brief/indexed projections remain available.
Conflicting predicates reject. Native bounds selection does not evaluate scalar
payloads or select register classes. Identity predicate kind 23 carries expected
identity 1 in its Class word and the same bounded arity/item fields. Register
projections 11/13 carry that identity in Literal's high word and the qualifier
ID in its low word. A prepared product is `$82,u8 payload length,u8 operator,
u8 left length,left leaf,right leaf`; operator 20 is shared numeric multiplication.
Leaves retain numeric names or compiled scalar wrappers, with no pointers, nested
product nodes or source-text fallback. Register extraction accepts either proven
identity side; the canonical match predicate prefers a successfully evaluated
right side before considering the left. An unresolved right scalar retains the
strict barrier rather than authorizing left fallback. All 64 bits matter.
Unsupported rows retain exact canonical match arity through RequiredForms
nibbles 10/11 (two/three items). A complete tuple with a different arity or an
already-proven complete non-tuple root disproves those rows; unknown structure
and contradictory predicates remain closed. The represented nested full-extension paths
are executable; other path
operations, terminals and call-child forms remain explicit unsupported boundaries. Typed
scalar/wrapped-value and
numeric tuple-name projections retain addressing predicates without source text
or CPU-specific native parsing. Its header is 228 bytes, with
big-endian block-relative fields. The canonical target identity remains at
offset 124 (length at 128); the preparation-only file plan offset and length are
at 132 and 136. Built-in `.emit` identity/CPU word width are at 140/142; its
shared data-plan offset/length at 144/148 remains in the runtime prefix. The
preparation-only inline metadata program is at 152/156. Contextual member-binding
offset/count at 160/164 select eight-byte rows derived from canonical selector
projections. Head-policy offset/length are at 168/172, PRVM version at 176 and
a zero reserved word at 178. Declaration-plan offset/length are at 180/184, its
PRVM version at 188 and a zero reserved word at 190. Numeric state-plan
offset/length are at 192/196. Expression-program offset/length are at 200/204,
with no EXVM version field. The core `.len` name ID is at 208 and the word at
210 is reserved zero. The shared numeric PRVM loop plan uses offset 212 and length
216 for counted/iterable `.for` and conditional `.while` envelopes. The shared
BranchPlan uses offset 220 and length 224 for `.if`/`.elseif`/`.else`/`.endif`.
Dictionary
role bit 3 marks core builtins, excluded from
ordinary lexical-name lookup. Dot-call binding resolves builtin names before
EXVM execution; generic call trees still carry opaque numeric IDs. The word at
offset 30 of each 44-byte candidate row is a
one-based state guard, zero when unguarded. Dictionary role bit 2 is reserved
for state-argument spellings and excluded from ordinary name lookup. These
policies and the state plan stay inside the retained RuntimeBytes prefix.
Shared PRVM entry 9 validates the labelled
scalar declaration envelope using the package-supplied `.const`/`.var`/`.set`
identity-to-role table and the numeric colon/equal operator policy, returning
operand spans plus immutable/mutable/conditional/update roles. The retained program
is 116 bytes, with 18 packed recipes for 17 punctuation update actions and
one contextual package-name identity for scalar `x=` repetition;
`:=`, `.var` and `.set` share the mutable role. Operator matching precedes the
optional label colon and consumes the complete packed record, independently of
the two-token physical-head hint. Scalar `:?=` is under qualification and has an
unresolved forward-initializer semantic choice. The punctuation compound updates
and scalar `x=` are covered by the shared declaration policy described below. A generic
adapter lowers the record after template
expansion and before scope/conditional processing; configuration capture lowers
its private writer record before import-parameter evaluation. Discovery selects
declaration roles through package identity before that materialization; it does
not decide declaration grammar. Scalar compilation and binding remain shared. Immutable graph evaluation
and layout consistency remain with the assignment/dependency owners; mutable
records (tag 43, or update markers 46–63) execute at their statement positions
and overwrite both signed64
words. Existing Defined byte states distinguish mutable ownership (3) from
precomputed absolute immutable values (2) and resolved runtime absolute snapshots
(4), without a new per-name allocation. Dependency
preparation alone may set record flag 64 on immutable declarations whose graph
reaches a mutable value; their expressions/bodies remain unchanged. Only these
readonly snapshots can retain unresolved pass-one placeholders and refresh in
pass two. Ordinary immutable layout checks stay strict. Complete lexical instruction
operands are normalized before binding
using those package-supplied field identities; shared directives and exact
register spellings retain their identities. MemberShape predicates and TargetMember
fixups operate on numeric wrappers without source text. Shared PRVM entry 7
selects output/descriptive roles and bounded decoded-string spans. Its current
BIN/FILL operand operations also publish numeric inclusive bounds, original
range-start spelling and fill bytes. Preparation owns the output spec vector;
output policy resolves names and validates the complete vector before writing.
The flat renderer clips/pads numeric spans without re-reading source strings.
This qualifies dotted literal metadata, not blocks, CPU-qualified overrides,
listing provenance, CLI range/fill options or deferred output configuration. Rust
preparation expands only active `.incbin` statements for
the quoted relative native subset, using definition-file-relative roots
and the explicit supported cases. Per-record origins for macro bodies drawn
from several physical files remain unqualified. Unsupported candidate recipes stay explicit rows rather than
becoming silent omissions. Package rows select package-owned recipes, numeric
projections and literal constants, including instruction encodings. Generic
native executes them without inventing CPU/family semantics. Preparation-only
markers are consumed before runtime records are emitted. If a bytecode or
package contract changes, regenerate its producer and migrate Rust/native
consumers together. Keep one current contract rather than a compatibility
executor for a superseded format.

Prepared binary assets use an opaque, length-prefixed byte token. Record walkers
validate its bounds and skip or copy the payload; emission bypasses text encoding
and macro substitution. This repairs `.incbin` payloads containing arbitrary
bytes, including bytes resembling identifiers or placeholders. The complete
embedded self-build has fresh exact native qualification above.

The intended data representation is raw bytes wherever values are already known:
resolve literals and active string encoding once, then retain the resulting bytes
for subsequent passes. Unresolved expressions and relocatable addresses still need
numeric expression/fixup records until their values and provenance are established.
Extending this compaction beyond binary assets is later work; memory and speed
benefits require separate before/after measurements.

For Hunk output, section and symbol provenance survives stabilization. The
native output path must preserve absolute instruction fixups and supported
CODE/DATA/BSS references, while shared DATA emission preserves `.word` signed
and unsigned range behavior. A numeric value has no relocation; an address
requires its complete relocation identity. Unsafe address arithmetic and an
unrepresentable alias must reject without emitting partial relocation data.

## Module and macro subset

The focused subset covers explicitly ordered physical files, module-local
imports, dependency discovery/order, selected includes, common `.use` forms,
visibility, public qualified references, selected-file discovery, and the
tested direct per-item, wildcard and scalar/flat-list/range configured
parameters. Numeric identities are bound before assembly; native assembly does not return to source
string lookup. Fresh exact Rust/native cases include missing and ambiguous
imports, cycles, private names, invalid include paths, aliases, and imported
macro calls.

`.use ... with (...)` Rust preparation evaluates known source-site scalar,
flat-list, range, struct type and struct-instance values. It captures schemas
through ordinary struct handling and carries typed snapshots into dependency
assembly, preserving nominal identity without generated assignment text.
Schemas with unknown sizes or conditional choices cannot supply parameters.
Native's focused flat-list/range
path uses shared EXVM and independent offset arenas, and covers `.len`, indexed
conditionals and mutable `.var`/`.set` snapshots. Native compound values also
pass focused forwarding through two imported modules and repeated equal imports.
Iterable-loop execution has separate partial qualification. The previous
checkpoint exposed imported iterator shadowing; the binding repair now passes
fresh shadowing, qualified-reference, restoration, register-role and growth
checks on both packages. Native configuration captures available schemas through
shared struct lowering. Types and instances cross independent offset arenas;
nominal and field identities are rebound before semantic replay. Source-site
member operands use the same numeric occurrence plans as ordinary expressions,
with visibility checked before publication. Top-level forward opening expression
guards now retain numeric branch envelopes for shared runtime replay. Mutable
replacements and updates execute only in selected branches; read-only dependency
ownership survives an inactive attempted replacement. Preparation-only effects
and the existing ordinary-loop restrictions remain unchanged.
Direct wide literal transport has separate focused qualification below. These
focused cases do not establish full Rust/native parity or a new full self-host
result; complete configuration parity remains unqualified.

Macro descriptors preserve definition order, lexical distance, visibility,
source order and argument spelling. Duplicate definitions and import/value
visibility conflicts reject according to Rust precedence. Numeric value
remapping is independent from template lookup and template visibility. A
per-item import alias currently renames the value; the macro remains callable
under its declared name. Module aliases and qualified macro calls are separate
supported cases.

Anonymous macro invocation scopes retain lexical hygiene through a
preparation-only marker. They must not become named reachability spans, because
selected imported expansion may otherwise discard emitted bytes. This marker
does not enter runtime records or alter the package format. Named blocks nested inside
anonymous macro scopes remain a distinct Rust/native selection edge.

## Implemented behavior retained

- BS18 selects ordinary and dotted heads after bare/adjacent-colon labels through shared
  PRVM policy before binding. Focused split/bare/colon controls pass on 6502 and 68020;
  member Hunk, composed macro label and register-spelling label regressions also pass.
  This does not establish complete family or language parity.
- Parent-path resolution recognizes Amiga volume separators, allowing textual and binary
  inclusion from a volume-root entry such as `Work:main.asm`. Root lookup, normalization
  and authorization retain their existing boundaries. Definition-relative acquisition
  remains bounded by the source-control subset above.
- BS19 lowers labelled scalar `.const` through shared PRVM into immutable assignment
  records, including configuration-time import parameters. This does not establish full
  declaration or 8085 parity.
- BS20 adds shared scalar `.var`/`.set` declaration roles and statement-time signed64
  storage with readonly snapshot tracking. Focused proof covers flat outputs, both label
  styles, conditionals, macro locals and instruction operands. Single-source-sweep
  concrete layouts remain supported. Hunk traversal now executes statements once per
  pass in source order, with bounded section-local cursors and independent output
  ordering. Unselected sections execute state without contributing output. Only
  resolved, proven-absolute readonly snapshots acquire runtime absolute state 4; their
  values and proof refresh each pass. One/two-map layouts still reject active mutations
  before filtered sweeps can reorder state, with dedicated-diagnostic controls. Mapped
  preparation now transfers discovered maps with canonical identity rebinding and
  lexical ownership before dependency bodies run; readonly mapped output, parameterized
  imports and inactive imports pass. Each seeded map must match its ordinary import
  replay once. The native two-map limit and late-map rejection remain explicit gaps.
  Mapped source-order traversal remains a separate structural slice. Compound-expression
  and scalar narrowing behavior are described below.
- TKVM-selected number normalization, composed-name recipes, and ordinary macro/segment
  string fragment recipes are implemented. Generated-call argument fragments are
  re-tokenized under VM control. Macro descriptor services use the compact PRVM boundary
  and offset-only fragment records.
- Preparation binds scopes, module identities, selected imports, visibility and
  supported scalar/flat-list/range parameters before assembly. Selected-file discovery
  and dependency order, common `.use` forms, selected includes and numeric import
  identities have fresh focused native/Rust coverage.
- Counted packed `.for` replay passes focused real-native comparison. Package-selected
  branch width, supported register masks, scalar roots, predicates, and supported
  instruction/data Hunk relocations are exercised by exact focused native/Rust
  comparisons.
- Imported anonymous macro invocation scopes carry a preparation-only marker so their
  expansions remain reachable without creating a named reachability span. The marker
  does not enter runtime records or change the package format.
- Template declarations and numeric value declarations have independent ownership and
  visibility. Per-item selection aliases expose the renamed spelling for both values and
  templates while retaining the canonical target identity. Module aliases and qualified
  selected macro calls remain separate forms. Focused coverage does not establish
  general module-alias parity.

The compact path has no general capability claim from those examples. Each
instruction recipe, expression form, import form and layout behavior needs
focused fresh Rust/native proof at its owning boundary. The full self-host result
at the top qualifies its recorded source tree on its recorded profile; it does
not turn unsupported language forms into supported ones.

These statements describe the implemented subset. They do not imply complete assembler-language parity.

## Remaining boundary gaps and explicit limitations

- Focused comparisons qualify complete FPU all-modes, instruction catalog, FPU
  registers, full-extension and PFLUSH/FPU Hex examples. Other path operations and
  terminals remain bounded by the numeric-path contract; these files do not establish
  whole-family parity.
- BS27 supports three top-level operands and indirect-register call-child projections.
  Transformed CAS2 execution probes match Rust, but the original colon-pair syntax still
  needs package/VM-owned normalization. These probes do not qualify unchanged-source
  CAS2 parity.

- Dotted call preparation compiles extra argument leaves that canonical call-register
  projections ignore. Unused nested calls, long strings or unresolved names can reject.
  BS24 covers selected register arguments and supported extra scalar/register leaves,
  not general call-expression parity.

- `binary_templates.rewriteCallText` still consumes decoded-string bytes in the residual
  body-token fallback. Replace this last caller with explicit VM-selected recipes before
  claiming complete string-boundary migration; keep decoded-string provenance for
  diagnostics and generated spelling.
- `binary_expression.compile` now runs the shared EXVM program, replacing its
  handwritten precedence parser. Fresh native component, folded-statement and
  session-lifecycle comparisons pass. Current full self-host evidence covers the
  identified `7a903cf8` source above; complete language parity remains open.
  Ordinary Rust expression handling still uses core token spelling instead of
  portable numeric metadata. Expression-range and operand-wrapper choices in
  preparation need a PRVM/package audit. Other native VM contracts still carry
  version fields/checks pending consolidation; their presence is a migration gap.
- Iterable `.for` and iterator-dependent conditionals have the focused BS33
  coverage [described above](#repetition-and-conditional-execution).
  Imported-shadow and qualified-reference probes, register-role separation,
  identity growth and scalar misuse rejection also have focused proof.
  BS34 `.while` has focused flat, whole-Hunk and two-concrete-section proof on
  both packages.
  Mapped modes 2 and 4 reject any `.while` until source-order traversal is repaired.
  Scoped `.bfor`/`.bwhile` have subsequent focused coverage described above; broader
  loop/macro parity remains open. Labels inside
  active unscoped loops reject. Counted packed-loop execution has focused
  native/Rust comparison.
- Named blocks inside anonymous macro scopes remain a separate Rust/native selection
  edge. Preserve the focused controls when changing macro selection.
- Compact projections remain bounded: supported fixups retain one section base plus an
  absolute addend; multi-base, non-affine or section-dependent addend algebra and
  genuine unsupported member-form targets must reject. Non-absolute layout aliases
  retain relocation identity and reject when output cannot represent them. Do not
  broaden expression or member behavior through a generic native shortcut.
- `.use ... with (...)` Rust preparation supports known source-site scalar, flat-list,
  range, struct type and struct-instance values. Typed snapshots preserve schema
  identity across dependency ordering. Native has focused flat-list/range coverage using
  shared EXVM, independent offset arenas, `.len`, indexed conditionals and `.var`/`.set`
  snapshots; compound forwarding through two imported modules and repeated equal imports
  have focused native coverage. Iterable-loop execution has separate partial
  qualification; the imported-shadow repair has fresh matching probes on both packages,
  including qualified-reference preservation. Native struct configuration has focused
  type/instance, member, snapshot and forwarding proof; top-level forward opening
  guards, unresolved alternatives following known false arms, and selected mutable
  updates now use numeric replay. Rust also stabilizes loop presence under certified
  immutable scalar forward guards before emission; contextual guards and retained-loop
  count changes keep their strict checks. Rust guard/count resolution admits one
  certified branch-local scalar dependency batch; later count-expanded declarations and
  new guard activation chains remain unsupported. Native now reuses numeric PRVM branch
  selection and the shared scalar dependency resolver for top-level conditional
  declarations, with focused two-package guard/count coverage. Inactive alternatives
  remain dormant; selected duplicate ownership rejects. Focused typed-owner coverage
  also qualifies lists/ranges and source-order aliases consumed by iterable loops after
  a certified forward guard, including corrected nested literal traversal.
  Mutable/contextual dependencies and further guard activation remain restricted. These
  cases do not establish full module, parameter or Hunk-expression parity. The signed
  scalar iterable checkpoint below covers negative/wide elements separately.

For instruction and address selection, required tuple/register classes and
package predicates remain barriers: native may skip a higher-priority recipe
only after a package-owned structural proof shows that recipe cannot match.
Unknown shapes and matching unsupported recipes still block. Register masks
cover the proved name/range/join forms; cross-class ranges, overflowing package
ordinals and a genuinely matching member target remain rejected. The proof is
about package-selected structure, never a new native opcode rule.

For expressions, named absolute addends and supported immediate-memory
relocations have focused exact cases. Do not infer that all algebra preserves
relocation: supported one-base absolute-addend fixups do not imply support for
multi-base or unsafe address arithmetic. A numeric or flat-label value with no section provenance can be
treated as scalar where Rust does so; a defined non-absolute layout alias must
not lose its identity merely because section metadata is absent.

Undefined expression identities are provisional only in pass 1 and must resolve
by pass 2. In relocatable output, a defined non-absolute alias without usable
section provenance rejects; a flat label with no section identity may remain
scalar. Preserve this distinction when adapting expression and fixup handling.

## Focused native comparison commands

Run native comparisons with the configured FS-UAE environment, serial test
execution, a fresh Rust oracle, and the same source bytes. The physical-line
boundary suite remains the reusable check for refill splits and the fixed
4,096-byte line limit:

```sh
cargo test -p asm compact_physical_line_ -- --ignored --nocapture --test-threads=1
```

The full current-source readiness command and its evidence requirements are at
the top of this note. Keep small exact-output boundary cases alongside it when
changing package selection, macro visibility, strings, expressions, loops or
Hunk relocation. Negative expected exits establish only the specific rejection
contract; they are not successful artifact parity.

## Stable correction anchors

The following anchors remain for active documentation links. They summarize current
behavior rather than preserving the old checkpoint narrative.

### Numeric normalization checkpoint

<a id="numeric-normalization-checkpoint"></a>

TKVM selects numeric normalization policy and emits normalized unsigned 64-bit
metadata or deferred invalid/overflow state. The raw writer retains five-byte
`[2, BE u32]` tokens when the high word is zero; otherwise it emits nine-byte
`[64, BE u64]` tokens. Packed data/macro services and source walkers treat the
payload as opaque, bounded bytes. Shared EXVM maps both forms to its existing
numeric terminal and validates Rust's signed i64 literal limit when compiling
an expression. Literal tree nodes use the existing First/Second fields for
low/high words, so node size and the evaluator's existing i64 representation do
not grow. All serialized references remain offsets or numeric identities.

Preserve numeric-looking names and substitution syntax until context establishes
that a value is required. Direct positive literals end at `i64::MAX`; the minimum
signed value can be expressed as `-9223372036854775807-1`. Shared scalar data
narrowing now matches Rust: low-u32 conversion precedes byte/word/long emission;
byte truncation warns and retains layout, while `.emit` remains strict. The
application reports final-pass warnings through retained numeric provenance,
and `--Werror` fails after writing outputs. This does not qualify all diagnostics
or output formats.
The portable token record layout and package opcode contract are documented in
the referenced VM sources.

References: [portable token contract](../../crates/opforge-vm/src/portable_contract.rs),
[Rust tokenizer helpers](../../crates/opforge-vm/src/tokenizer_runtime_utils.rs),
[native scanner](../../native/motorola68000/amigaos/tkvm/tkvm_scanner.asm), and [package
generation](../../crates/opforge-vm/src/builder.rs).

### Ternary expression selection

The compact path now compiles `condition ? then : else` through the shared EXVM
ternary action and the existing canonical ExprVM selector. It retains the
selected scalar, list, range or record shape and its section provenance. Both
arms are evaluated eagerly, matching Rust; invalid conditions, unavailable
symbols and arithmetic errors still reject. Ternaries use the existing typed
wrapper, while ordinary scalar expressions retain their compact folding path.
No node/cell layout or package contract changes. This has focused native coverage.
Full expression, assembler and self-host qualification are separate claims.

### Struct schemas and literal construction

The shared EXVM struct production identifies field keys before ordinary reference
binding. Accepted FIELD nodes annotate numeric keys; source lookahead only selects
candidates for that grammar. Keys do not acquire import or reachability edges.
Literal lowering emits the existing canonical ExprVM struct constructor with a
nominal type ID and bare member IDs. Canonical binary operands map to the native
calculator at the typed evaluator boundary; private compact operands retain their
existing representation. This is a representation distinction, not VM version
selection.

`binary_struct_values` owns schema/literal construction and member validation.
Schemas retain numeric nominal identity, total size and field geometry. Literal
fields retain signed scalar cells and raw VM provenance. Both use owner-relative
arena offsets; neither is an address record with an implicit default. Immutable
schema seeds survive value rewind/reset and are released with their owner.
Declarations publish types in source order through an explicit private packed
marker. Copies retain nominal identity; final binding remaps type IDs and preserves
bare member IDs.

Assembler capture rejects relocatable instance fields, matching the current Rust
value model; temporary member access in the raw typed VM retains provenance.
Scalar preparation and execution share the original-definition size leaf rule,
while typed aliases and compound expressions remain non-scalar. This has focused native coverage.
This does not establish complete expression/assembler parity or fresh self-host
qualification.

### Signed scalar iterable checkpoint

Iterable `.for` and `.bfor` bind both words of the existing signed i64 scalar
cell. Shared EXVM constructs lists/ranges; the existing value service validates
scalar kind/provenance and obtains each element. The loop consumer no longer
rejects nonzero high words or clears them during binding. Saved outer iterator
cells and scoped value snapshots already retain both words, so their layouts
do not grow. Descriptor offsets and the 65,536-iteration bound remain unchanged.
That checkpoint covers scalar elements. The subsequent typed-list slice preserves
members and section identity when indexed, then projects record/address elements
to absolute scalar iterator bindings in `.for` and `.bfor`. Absolute scalar lists
remain compact; descriptors, owner-relative offsets and loop-slot sizes are unchanged.
The assembler adapter rejects nested list/range elements while raw shared ExprVM
evaluation retains compound shapes. This has focused native coverage.

### Macro descriptor service checkpoint

<a id="macro-descriptor-service-checkpoint"></a>

Macro definitions and calls use package-selected PRVM services and bounded offset-only
descriptor/fragment records. Generated calls re-tokenize selected argument fragments;
the residual decoded-string fallback above remains. Preserve lexical hygiene, source
order, visibility and the separation between macro identity and numeric value identity.

Scalar compound assignments use the shared PRVM declaration policy and
numeric update markers. They reference existing mutable scalar identities,
including earlier lexical ancestors; they do not declare new locals. Arithmetic
and preparation snapshots share the ExprVM assignment service. Dictionary role
bit 4 marks the package-owned `x` grammar word; the shared PRVM recognizes it only
after an update target and before an equals token. The writer supplies raw
lookahead metadata, and binding retains ordinary source/register/member facets
elsewhere, including extended precision `.x`. Assignment RHS contexts use that
same classifier, so register-named symbols can remain lexical values.
Immutable configuration captures marshal that same numeric identity into the
shared classifier. No frontend spelling comparison or execution-time text lookup
is added. Repeat execution uses the left scalar's low byte, repeats it up to four
times using the RHS low-u32 count, and returns a zero-extended u32.

## Current implementation owners

| Area | Current owner | Boundary to preserve |
| --- | --- | --- |
| Portable token contract | [portable contract](../../crates/opforge-vm/src/portable_contract.rs) and [Rust scanner helpers](../../crates/opforge-vm/src/tokenizer_runtime_utils.rs) | Token identity and optional number metadata. |
| Native tokenization | [TKVM scanner](../../native/motorola68000/amigaos/tkvm/tkvm_scanner.asm) and [program builder](../../crates/opforge-vm/src/builder.rs) | Package-owned lexical classification and normalization policy. |
| Native frontend and preparation | [frontend](../../native/motorola68000/amigaos/experimental/binary_frontend.asm), [preparation](../../native/motorola68000/amigaos/experimental/binary_prepare.asm), [writer](../../native/motorola68000/amigaos/experimental/binary_source.asm) | Token use, identity binding, recipe selection and packed-source output. |
| Macro fragments and descriptors | [templates](../../native/motorola68000/amigaos/experimental/binary_templates.asm), [macro plans](../../native/motorola68000/amigaos/experimental/binary_macro_plans.asm), and PRVM package producers | Offset-only descriptors, lexical hygiene and selected string/argument recipes. |
| Bounded product nodes | [products](../../native/motorola68000/amigaos/experimental/binary_products.asm) and tuple preparation | Numeric node framing; scalar identity proof through ExprVM, with target choices in package projections. |
| Expression parsing and execution | [native expression compiler](../../native/motorola68000/amigaos/experimental/binary_expression.asm) and shared ExprVM | Shared EXVM owns syntax; scalar/typed lowering emits bounded ExprVM programs. |

These are responsibility references, not an exhaustive source or instruction
audit. CPU/family semantics stay in package definitions and their specialized
implementation layers. Shared `.byte`, `.word`, `.org`, and other generic
directives remain in shared processing; unresolved dot-prefixed names cannot
fall through to instruction handling.

### Native proof boundary

The maintained proof contract is [native Rust
parity](../../agents/rules/native-rust-parity-porting.md). Fresh guest completion is
mandatory: timeout, crash, partial capture, or launcher success is not completion.
Compare against a live Rust build from identical source bytes. The full run recorded at
the top qualifies that earlier source state; later source changes require a fresh full
proof.

## Related current documentation

- [Native assembler completion plan](native-runtime-reset.md) owns the 68020 / 2 MiB
  product goal, memory direction and broader integration boundaries.
- [Workflow guide](../workflow/README.md) describes validation and artifact lifecycle.
- [Canonical VM boundary](../vm-boundary-protocol-v1.md#3-canonical-boundary-matrix)
  defines which VM owns each grammar and operation.
- [Package execution boundaries](package-execution-boundaries.md#outcome-and-scope)
  defines permitted shared execution primitives and package ownership.
