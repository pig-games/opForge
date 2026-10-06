# Compact frontend: VM boundary correction

Status: BS27 transports three numeric operands, wrapped call-register children,
register/immediate pairs and register triples. Same-source PACK/UNPK, LINK and
CAS checks and transformed CAS2 execution probes pass. The last full
embedded compact self-host proof precedes the layout follow-up below and has
exact whole-Hunk Rust/native equality. This qualifies that implementation and
case, not whole-language or CPU-family parity. The [current slice](native-runtime-reset.md#three-operands-and-indirect-call-children-bs27-checkpoint)
records focused coverage, costs and remaining boundaries.

## Current breadth parity checkpoint

The [BS27 corpus checkpoint](native-runtime-reset.md#current-breadth-parity-checkpoint)
records all 167 selected roots: MOS 20/39 exact positives, M68K 21/36 and opcore
17/49 (four opcore positives cannot produce a live Rust oracle). Of 43 expected
errors, 41 have completed native rejection observations; undefined-region
placement and a forward-count loop are accepted
by native while Rust rejects them. Negative completion is not diagnostic parity.
The corpus exposes two exit-zero positive Hex mismatches: MOS branch offsets and
opcore alignment gaps. No production semantics or references change in the audit.

The subsequent Rust qualified-scope repair restores all four original CLI roots;
it changes shared Rust lookup and deferred reachability, with no native/package
changes or new native coverage. The corpus counts above remain its original
observations. The subsequent
[native layout checkpoint](native-runtime-reset.md#native-region-validation-and-alignment-provenance)
qualifies fifteen fresh focused cases: undefined regions reject, forward region
declarations remain valid, flat alignment gaps stay sparse in Hex/S-record, and
placed payload padding remains initialized as in Rust. Bin/Hunk and 6502 package
checks pass. The original corpus counts above are not a rerun of the full inventory.
The subsequent [immutable-count repair](native-runtime-reset.md#immutable-forward-loop-counts)
aligns Rust with native acceptance of graph-proven forward scalar counted loops.
It preserves strict traversal checks for layout-dependent changes, source-order
snapshots and iterator shadowing. Newly activated constant declarations still
reject; forward `.while` limits and deferred iterables are outside that slice.
The freshly Rust-built native executable exactly matches the layout checkpoint's
535,024-byte image. Seven focused current native cases qualify across the initial
batch and a snapshot rerun using `.var`/`.set`; the initial `:=` control exposed
a separate unsupported native assignment form. These follow-ups have no new full
native self-host run.
Complete PFLUSH/FPU and full-extension sources now match; original colon-pair
syntax still blocks DIVS/CAS2 and the MOVE16 carry-forward example before MOVE16.
Current full self-host proof remains the separate result below.

## Most recent full self-host qualification (BS27)

The complete BS27 embedded implementation self-assembles in FS-UAE with fresh
case-bound START/DONE, native exit zero and exact live Rust equality for the
entire 534,964-byte Hunk. Bootstrap and output embed only the 394,318-byte
m68020 package; 103 mapped source/binary inputs total 1,509,170 bytes. Linked
static reservation is 554,164 bytes, not peak RAM. Source fingerprint is
`fnv1a64:87ef757925cb6aba`, package fingerprint `fnv1a64:0e13af9d947d7620`, and
bootstrap/output fingerprint `fnv1a64:a0c63661a3b96bb1`.

Release START/DONE takes 1121.611579667 seconds (18m41.61s), versus BS26's
1114.496267083 (18m34.50s): +7.115313 seconds (+0.6384%), one full sample per
state with different source/package bytes. The unchanged indexed workload on
68020/10 MiB separately averages 12.428726354 seconds versus 12.3745079375
(+0.4381%, two samples per state). Intermediate label, pair and triple changes
have separate measurements in the current slice; small indexed differences are
within the observed variation. Keep the two input sets and memory profiles
separate. Full self-host uses 68020/74 MiB; no physical A6000 timing, complete
peak-memory capture or 2 MiB fit is claimed.

The previous BS26 reference Hunk was 518,896 bytes with 538,140 bytes of linked
reservation and a 378,874-byte embedded package. BS27 adds 16,068 Hunk bytes,
16,024 linked-reservation bytes and 15,444 package bytes. Its numeric-path
qualification remains recorded in the [BS26 slice](native-runtime-reset.md#numeric-expression-paths-bs26).
Only latest BS27 packages are accepted by current code; older bundles are
baseline evidence. Raw CAS2 colon normalization, other path/call-child forms,
unused extra call-argument preparation and localized
import-alias/local-struct shadowing remain open.

The maintained full embedded proof uses `export_compact_self_host_bundle` with
`OPFORGE_COMPACT_EXPORT_OUTPUT_EMBED=68020`,
`OPFORGE_COMPACT_EXPORT_NATIVE=1`, a new absolute
`OPFORGE_COMPACT_EXPORT_DIR` and the [configured FS-UAE environment](../../agents/rules/fs-uae.md).
Host export alone does not establish native completion. Fresh case-bound
START/DONE, explicit exit zero and exact complete Hunk equality are required;
stored manifests or outputs cannot replace the live oracle.

## Physical A6000 execution

The qualified release bundle `/tmp/opforge-selfhost-bs27-qualified-release-74m`
is selected by `/tmp/opforge-a6000-current`. Bootstrap and assembled output both
embed only `m68020--motorola68k.bin`. The named package is retained for local
identity verification; no external runtime fallback is used. This bundle has
complete FS-UAE qualification and a passing local transfer-preparation dry run,
but no physical execution or remote transfer proof.
Use macOS Terminal, where `ash` and `acp` can reach the A6000:

```sh
python3 /Users/erik/Code/Retro/opForge/scripts/performance/run_a6000_selfhost.py
```

Defaults are host `192.168.0.220`, volume `Development` and a one-hour assembly
timeout. Each invocation creates its own remote directory and local result tree.
The script validates the current BS27 package header, mapped source preamble and
exact source/package/image bytes before transfer, then round-trips all inputs
before execution. Filename components must fit the 30-byte classic limit.
Guest `Date` brackets assembly at one-second resolution, excluding transfer;
host command time includes connection and Shell setup. Success requires fresh
case-bound markers, explicit guest exit zero and exact full-Hunk output. A timeout
can leave a guest command running: inspect that run before retrying.

The separate historical instrumented bundle `/tmp/opforge-selfhost-bs20-instrumented`
completes its entire BS20 baseline self-host with fresh exact live Rust output, in
1,147.941797667 seconds. Preparation is 665.36 seconds, assembly 481.54 seconds;
peak tracked allocation is 22,380,568 bytes with zero terminal ownership, balanced
allocated/freed capacity and zero profiling/allocation errors. Two source sweeps
execute 122,798 record visits. Its transfer dry run also passed at that checkpoint. Regenerate this
instrumented configuration as BS27 before selecting it in the current hardware
runner; the stored BS20 bundle is baseline evidence only.
It targets the identical release output and source/package case, enabling memory, phase/progress, sampled
binding, template and input probes. `OPFORGE_PHASE_ONLY=1` excludes detailed
per-opcode tokenizer probes. The exporter retains fresh native stdout/stderr and
`memory.bin`, enabling traversal-counter decoding after ephemeral runner cleanup.
Instrumentation time is reported separately from release time. Hardware results
separate assembly success from telemetry validity; balanced ownership, zero
profiling/allocation errors and valid clocks are required for valid measurements.
Nested binding/input observations must not be added to exclusive stage totals.

The earlier physical baseline remains useful context: the 61-file implementation
produced the exact 89,880-byte Rust Hunk twice in 15 seconds at one-second guest
resolution. Its full-token instrumented run completed in 58 seconds, measuring
49.16 seconds preparation, 8.82 seconds assembly and 5,222,440 bytes peak tracked
ownership with balanced cleanup and no profiling/allocation errors. Those inputs,
packages and probe settings differ from BS20, so they do not establish a code-change
speedup or current memory needs. Detailed superseded captures and export instructions
remain in Git history.

## Ownership and representation contracts

The compact path is a package-directed execution path. Rust and the canonical package define semantic behavior; native code executes bounded shared primitives and package-selected operations. Update package producers and consumers together and retain only the latest supported format; version identifiers detect mismatches and do not require compatibility executors.

| Responsibility | Contract |
| --- | --- |
| TKVM | Own line tokenization and package-selected lexical policy, including numeric normalization and explicit composed-name/substitution recipes. |
| PRVM | Own statement, operand, macro and other preparation grammar. Native code may validate bounds or execute selected primitives, but must not invent source grammar from text or packed offsets. |
| EXVM | Own expression parsing and evaluation for covered expressions. Native code may write or execute the selected expression program; a native precedence parser is not proof of VM ownership. |
| Native writer / assembler | Consume numeric identities, structured values, bounded expression programs and offset-based records. Preserve source provenance for diagnostics. Keep immutable preparation separate from mutable pass/layout state. |

Bootstrap and macro expansion may have host-owned steps where the VM boundary protocol allows them. The relevant contract is which component selects the grammar and owns its behavior, not whether native routines contain branches.

Persist offsets and numeric IDs, never process pointers. Every stored offset has an explicit base and validated bounds; moving a block must not require patching stored records. Scratch preparation storage is released before assembly. Hunk fixups retain complete symbol and section identity through stabilization and output; unsafe arithmetic and unsupported provenance fail closed. Generic directives and CPU/family semantics remain in their established shared or package-owned layers.

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

The compact capsule embeds TKVM and macro descriptor programs; it does not
embed an EXVM expression-parser program. Existing canonical expression
bytecode and shared ExprVM execution do not supply that missing parser program.
The macro-only initial PRVM plan runs after binding. A separate shared four-byte
PRVM prefix policy now runs before binding: BeginStatement,
ParseOptionalLeadingLabel, FinishLine, End. The VM owns column-one label and
adjacent-colon decisions. The native adapter presents two logical tokens and
maps the returned cursor to a physical token index, including composed-name
recipes; the writer uses that index to distinguish package heads from values.

BS27 is the current compact package format; the producer writes `BS27` and the
native package owner checks the matching magic. Only this latest runtime contract
is supported; packages must be regenerated. Candidate rows are 36 bytes: offsets
0–31 retain their previous fields, byte 32 carries the third-operand form nibble,
byte 33 the third tuple class (class + 1), and word 34 is reserved zero. Shapes
11/12/13 carry direct and immediate three-operand forms; 14 represents
register/immediate, 15 register/register/direct and 16 register/register/register.
These are structural distinctions; package projections own classes and semantics.
Projection kind 27 selects a
wrapped register child from call argument 0/1 on operand 0–2, using the same
class/value-program/argument fields as kind 24; no callee or opcode spelling is
interpreted by native transport. Raw colon normalization remains unsupported.
Target flags at 130 request structural
wrapper preservation (bit 0) and nested preparation (bit 1) from canonical projections. Tuple register/value/qualified projections use kinds 11/12/13. Descriptor
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
available. Deduplicated path programs occupy the arena between the 200-byte
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
and contradictory predicates remain closed. The represented nested full-extension paths are executable; other path
operations, terminals and call-child forms remain explicit unsupported boundaries. Typed scalar/wrapped-value and
numeric tuple-name projections retain addressing predicates without source text
or CPU-specific native parsing. Its header is 200 bytes, with
big-endian block-relative fields. The canonical target identity remains at
offset 124 (length at 128); the preparation-only file plan offset and length are
at 132 and 136. Built-in `.emit` identity/CPU word width are at 140/142; its
shared data-plan offset/length at 144/148 remains in the runtime prefix. The
preparation-only inline metadata program is at 152/156. Contextual member-binding
offset/count at 160/164 select eight-byte rows derived from canonical selector
projections. Head-policy offset/length are at 168/172, PRVM version at 176 and
a zero reserved word at 178. Declaration-plan offset/length are at 180/184, its
PRVM version at 188 and a zero reserved word at 190. Numeric state-plan
offset/length are at 192/196; the final word of each 32-byte candidate row is a
one-based state guard, zero when unguarded. Dictionary role bit 2 is reserved
for state-argument spellings and excluded from ordinary name lookup. These
policies and the state plan stay inside the retained RuntimeBytes prefix.
Shared PRVM entry 9 validates the labelled
scalar declaration envelope using the package-supplied `.const`/`.var`/`.set`
identity-to-role table and returns operand spans plus immutable/mutable ownership.
The retained program is 13 bytes; both mutable spellings share one role. A generic adapter lowers the record after template
expansion and before scope/conditional processing; configuration capture lowers
its private writer record before import-parameter evaluation. Discovery selects
declaration roles through package identity before that materialization; it does
not decide declaration grammar. Scalar compilation and binding remain shared. Immutable graph evaluation
and layout consistency remain with the assignment/dependency owners; mutable
records (tag 43) execute at their statement positions and overwrite both signed64
words. Existing Defined byte states distinguish mutable ownership (3) from
precomputed absolute immutable values (2) and resolved runtime absolute snapshots
(4), without a new per-name allocation. Dependency
preparation alone may set record flag 64 on immutable declarations whose graph
reaches a mutable value; their expressions/bodies remain unchanged. Only these
readonly snapshots can retain unresolved pass-one placeholders and refresh in
pass two. Ordinary immutable layout checks stay strict. Complete lexical instruction operands are normalized before binding
using those package-supplied field identities; shared directives and exact
register spellings retain their identities. MemberShape predicates and TargetMember
fixups operate on numeric wrappers without source text. Shared PRVM entry 7
selects output/descriptive roles and bounded decoded-string spans. Rust
preparation expands only active `.incbin` statements for
the quoted relative native subset, using definition-file-relative roots
and the explicit supported cases. [Focused native qualification](native-runtime-reset.md#bs13-binary-inclusion-qualification)
passes; BS13 embedded-config self-hosting is qualified in the current plan.
The BS20 baseline embedded full self-host is qualified on the expanded investigation
profile; per-record origins for macro bodies drawn from several physical files
remain unqualified. Unsupported candidate recipes stay explicit rows rather than
becoming silent omissions. Package rows select package-owned recipes, numeric
projections and literal constants, including instruction encodings. Generic
native executes them without inventing CPU/family semantics. Preparation-only
markers are consumed before runtime records are emitted. If a bytecode or
package contract changes, regenerate its producer and migrate Rust/native
consumers together. Keep one current contract rather than a compatibility
executor for a superseded format.

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
tested direct per-item, wildcard and scalar configured parameters. Numeric
identities are bound before assembly; native assembly does not return to source
string lookup. Fresh exact Rust/native cases include missing and ambiguous
imports, cycles, private names, invalid include paths, aliases, and imported
macro calls.

Scalar `.use ... with (...)` evaluation is limited to earlier module-scope `=`
or scalar `.const` constants and incoming parameters in the compact signed-32-bit
expression grammar. Preparation filters nested `.if`/`.else`/`.endif` records only when
their module-scope scalar inputs are known. The unsupported forms are listed
explicitly below; they are not implicitly covered by the imported-module tests.

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

- BS18 selects ordinary and dotted heads after bare/adjacent-colon labels through shared PRVM policy before binding. Focused split/bare/colon controls pass on 6502 and 68020; member Hunk, composed macro label and register-spelling label regressions also pass. The [current checkpoint](native-runtime-reset.md#bs18-shared-instruction-heads--focused-parity) records the selected corpus results and isolated time/size cost. This does not establish complete family or language parity.
- Parent-path resolution recognizes Amiga volume separators, allowing textual and binary inclusion from a volume-root entry such as `Work:main.asm`. Root lookup, normalization and authorization retain their existing boundaries. The [focused checkpoint](native-runtime-reset.md#bs18-volume-root-includes--focused-parity) records the original include proof and the subsequent `.const` stops; the BS19 checkpoint below records their retry.
- BS19 lowers labelled scalar `.const` through shared PRVM into immutable assignment records, including configuration-time import parameters. The [focused checkpoint](native-runtime-reset.md#bs19-shared-scalar-declarations--focused-parity) records the controls, selected real-example retries, contract migration and remaining compound-value/8085 gaps.
- BS20 adds shared scalar `.var`/`.set` declaration roles and statement-time signed64 storage with readonly snapshot tracking. [Focused proof](native-runtime-reset.md#bs20-scalar-mutable-declarations--single-sweep-checkpoint) covers flat outputs, both label styles, conditionals, macro locals and instruction operands. Single-source-sweep concrete layouts remain supported. [Hunk traversal](native-runtime-reset.md#bs20-source-order-hunk-traversal) now executes statements once per pass in source order, with bounded section-local cursors and independent output ordering. Unselected sections execute state without contributing output. Only resolved, proven-absolute readonly snapshots acquire runtime absolute state 4; their values and proof refresh each pass. One/two-map layouts still reject active mutations before filtered sweeps can reorder state, with dedicated-diagnostic controls. [Mapped preparation](native-runtime-reset.md#bs20-mapped-preparation--configuration-transfer-repair) now transfers discovered maps with canonical identity rebinding and lexical ownership before dependency bodies run; readonly mapped output, parameterized imports and inactive imports pass. Each seeded map must match its ordinary import replay once. The native two-map limit and late-map rejection remain explicit gaps. Mapped source-order traversal remains a separate structural slice. Compound values and general narrow-output truncation remain gaps.
- TKVM-selected number normalization, composed-name recipes, and ordinary macro/segment string fragment recipes are implemented. Generated-call argument fragments are re-tokenized under VM control. Macro descriptor services use the compact PRVM boundary and offset-only fragment records.
- Preparation binds scopes, module identities, selected imports, visibility and supported scalar parameters before assembly. Selected-file discovery and dependency order, common `.use` forms, selected includes and numeric import identities have fresh focused native/Rust coverage.
- Counted packed `.for` replay passes focused real-native comparison. Package-selected branch width, supported register masks, scalar roots, predicates, and supported instruction/data Hunk relocations are exercised by exact focused native/Rust comparisons.
- Imported anonymous macro invocation scopes carry a preparation-only marker so their expansions remain reachable without creating a named reachability span. The marker does not enter runtime records or change the package format.
- Template declarations and numeric value declarations have independent ownership and visibility. Per-item selection aliases currently rename values; selected macro calls retain their declared name. Module aliases and qualified macro calls are separate supported forms.

The compact path has no general capability claim from those examples. Each
instruction recipe, expression form, import form and layout behavior needs
focused fresh Rust/native proof at its owning boundary. The full self-host result
at the top qualifies its recorded source tree on its recorded profile; it does
not turn unsupported language forms into supported ones.

These statements describe the implemented subset. They do not imply complete assembler-language parity.

## Remaining boundary gaps and explicit limitations

- The [current breadth checkpoint](native-runtime-reset.md#current-breadth-parity-checkpoint) qualifies complete FPU all-modes, instruction catalog, FPU registers, full-extension and PFLUSH/FPU Hex examples. Other path operations and terminals remain bounded by the numeric-path contract; these files do not establish whole-family parity.
- BS27 supports three top-level operands and indirect-register call-child projections. Transformed CAS2 execution probes match Rust, but the original colon-pair syntax still needs package/VM-owned normalization. These probes do not qualify unchanged-source CAS2 parity.

- Dotted call preparation compiles extra argument leaves that canonical call-register projections ignore. Unused nested calls, long strings or unresolved names can reject. BS24 covers selected register arguments and supported extra scalar/register leaves, not general call-expression parity.

- `binary_templates.rewriteCallText` still consumes decoded-string bytes in the residual body-token fallback. Replace this last caller with explicit VM-selected recipes before claiming complete string-boundary migration; keep decoded-string provenance for diagnostics and generated spelling.
- `binary_expression.compile` still implements precedence and associativity in native code before invoking shared ExprVM. Move covered expression compilation behind package/EXVM ownership. Ordinary Rust expression handling also still uses core token spelling instead of portable numeric metadata. The expression-range and operand-wrapper choices in preparation need a PRVM/package audit as this boundary moves.
- Iterable `.for` and `.bfor` remain unsupported; labels inside an active unscoped loop reject. Counted packed-loop execution is supported and has focused native/Rust comparison. These limits do not imply that general loop execution is absent.
- Named blocks inside anonymous macro scopes remain a separate Rust/native selection edge. Preserve the focused controls when changing macro selection.
- Compact projections remain bounded: supported fixups retain one section base plus an absolute addend; multi-base, non-affine or section-dependent addend algebra and genuine unsupported member-form targets must reject. Non-absolute layout aliases retain relocation identity and reject when output cannot represent them. Do not broaden expression or member behavior through a generic native shortcut.
- Scalar `.use ... with (...)` support is limited to supported signed-32-bit expressions from earlier module-scope `=` or scalar `.const` constants and incoming parameters. Within this import-parameter evaluation model, compound values, loop-derived or other assembly-time-dependent conditions, and expressions outside the compact grammar remain unsupported. This limit does not describe general loop execution. Do not claim full module, parameter or Hunk-expression parity.

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

The following anchors remain for active documentation links. They summarize current behavior rather than preserving the old checkpoint narrative.

### Numeric normalization checkpoint

<a id="numeric-normalization-checkpoint"></a>

TKVM selects numeric normalization policy and emits normalized unsigned 64-bit metadata or deferred invalid/overflow state. The writer consumes values that fit its existing representation. Preserve numeric-looking names and substitution syntax until context establishes that a value is required. The portable token record layout and package opcode contract are documented in the referenced VM sources.

References: [portable token contract](../../crates/opforge-vm/src/portable_contract.rs), [Rust tokenizer helpers](../../crates/opforge-vm/src/tokenizer_runtime_utils.rs), [native scanner](../../native/motorola68000/amigaos/tkvm/tkvm_scanner.asm), and [package generation](../../crates/opforge-vm/src/builder.rs).

### Macro descriptor service checkpoint

<a id="macro-descriptor-service-checkpoint"></a>

Macro definitions and calls use package-selected PRVM services and bounded offset-only descriptor/fragment records. Generated calls re-tokenize selected argument fragments; the residual decoded-string fallback above remains. Preserve lexical hygiene, source order, visibility and the separation between macro identity and numeric value identity.

## Current implementation owners

| Area | Current owner | Boundary to preserve |
| --- | --- | --- |
| Portable token contract | [portable contract](../../crates/opforge-vm/src/portable_contract.rs) and [Rust scanner helpers](../../crates/opforge-vm/src/tokenizer_runtime_utils.rs) | Token identity and optional number metadata. |
| Native tokenization | [TKVM scanner](../../native/motorola68000/amigaos/tkvm/tkvm_scanner.asm) and [program builder](../../crates/opforge-vm/src/builder.rs) | Package-owned lexical classification and normalization policy. |
| Native frontend and preparation | [frontend](../../native/motorola68000/amigaos/experimental/binary_frontend.asm), [preparation](../../native/motorola68000/amigaos/experimental/binary_prepare.asm), [writer](../../native/motorola68000/amigaos/experimental/binary_source.asm) | Token use, identity binding, recipe selection and packed-source output. |
| Macro fragments and descriptors | [templates](../../native/motorola68000/amigaos/experimental/binary_templates.asm), [macro plans](../../native/motorola68000/amigaos/experimental/binary_macro_plans.asm), and PRVM package producers | Offset-only descriptors, lexical hygiene and selected string/argument recipes. |
| Bounded product nodes | [products](../../native/motorola68000/amigaos/experimental/binary_products.asm) and tuple preparation | Numeric node framing; scalar identity proof through ExprVM, with target choices in package projections. |
| Expression parsing and execution | [native expression compiler](../../native/motorola68000/amigaos/experimental/binary_expression.asm) and shared ExprVM | Current compiler is the unresolved native parser boundary; evaluation stays in ExprVM. |

These are responsibility references, not an exhaustive source or instruction
audit. CPU/family semantics stay in package definitions and their specialized
implementation layers. Shared `.byte`, `.word`, `.org`, and other generic
directives remain in shared processing; unresolved dot-prefixed names cannot
fall through to instruction handling.

### Native proof boundary

The maintained proof contract is [native Rust parity](../../agents/rules/native-rust-parity-porting.md). Fresh guest completion is mandatory: timeout, crash, partial capture, or launcher success is not completion. Compare against a live Rust build from identical source bytes. The run recorded at the top is the current-source baseline; any later source change requires a fresh exact proof.

## Related current documentation

- [Native assembler completion plan](native-runtime-reset.md) owns the 68020 / 2 MiB product goal, memory direction and broader integration boundaries.
- [Workflow guide](../workflow/README.md) describes validation and artifact lifecycle.
- [Canonical VM boundary](../vm-boundary-protocol-v1.md#3-canonical-boundary-matrix) defines which VM owns each grammar and operation.
- [Package execution boundaries](package-execution-boundaries.md#outcome-and-scope) defines permitted shared execution primitives and package ownership.
