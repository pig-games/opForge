# Binary-source native runtime

Status: native discovery selects module bodies within multi-module candidate files;
the experimental graph can omit unreachable imported named blocks and place two
mapped section pairs in adjacent literal regions.
Bounded native functional checks and release comparisons pass. The last broad host run
(F4) still had 160 baseline failures; this slice does not claim repository-wide
qualification. See the [migration plan](native-runtime-reset.md) for the remaining
language and product boundaries.
The compact native path completes the bounded mixed and expression workloads on a 68020 / 2 MiB guest.
It is not yet the normal native CLI and does not implement the full language. The
[native runtime migration plan](native-runtime-reset.md) defines the breadth-first
work that follows. Git history retains the completed S1, S1M and M1–M8 experiment
logs and intermediate measurements.

## Representation and execution contract

The tokenizer consumes source incrementally and writes length-prefixed binary
records. After preparation, assembly uses this binary source and the numeric
runtime-package view. Original text is not part of execution and may be reread only
for diagnostics.

- Names are normalized and interned during preparation. Package-owned aliases for
  one instruction share an identity; meaningful size or form qualifiers are stored
  separately. IDs are not machine opcodes.
- Records, expression programs and package data contain numeric IDs and offsets
  from defined bases, never process pointers. Nested offsets are allowed when the
  enclosing region is explicit. Bounds and arithmetic overflow are checked before
  resolving them.
- Expression structure is compiled once into compact numeric programs. Values,
  symbol definitions and the program counter remain mutable pass state and are
  evaluated when needed.
- Package semantics remain authoritative. Generic directives such as `.byte`,
  `.word`, `.long` and `.org` remain shared core behavior.
- Lexical dictionaries, source buffers and preparation scratch are released before
  replay. Compact source locations remain for diagnostics. Growth/copy overlap is
  included in owned-memory accounting.
- The current BSP3 capsule is an experimental host-to-native vehicle. It is not a
  persistent source-file or runtime-package format. Producer and consumer move
  together; earlier capsules are rejected and no compatibility executor is retained.

The design intentionally leaves room for an editable tokenized source format on
constrained platforms. Such a format would need optional spelling/format metadata
and a stable vocabulary identity, but its execution records would still use
offsets and numeric IDs. No disk-format contract has been adopted.

## Current implementation coverage

The current path supports one selected package pipeline; package-defined
registers, register pairs, indexed and immediate operands; parenthesized member operands;
column-one labels with optional colons, standalone or before statements; forward branches;
the program counter; parentheses; unary `+`/`-`/`~`; checked signed addition,
subtraction and multiplication; bitwise `&`/`|`/`^` and shifts `<<`/`>>`; and shared `.cpu`, `.org`, `.byte`, `.word`,
`.long` and `.end` directives. It performs a fixed two-pass layout and emission
over contiguous output.

The bounded `name = expression` form now supports forward absolute constant
chains. Before layout, F3 indexes definitions and walks numeric expression
references with an explicit dependency stack. Each absolute constant evaluates
once, after its dependencies; cycles and missing symbols reject. Temporary
expression offsets never become published symbol values, and dependency scratch
is released before layout. Earlier-label and definition-site-PC expressions keep
the source-order two-pass check; forward layout dependencies remain unsupported.
The native route remains narrower than Rust and does not yet accept `.const`.
The compact evaluator preserves signed symbol values within its checked signed32
range. Shift counts follow the canonical `count & 31` rule; right shifts are
logical over the canonical 64-bit value and out-of-range signed32 results reject.

Named `.block` scopes and `.endblock`/`.bend` now support nesting, parent lookup,
absolute qualified references and forward local shadowing. Preparation assigns
provisional IDs, records declarations and finalizes bindings before freeing the
scope/name dictionary. It rewrites IDs in packed records and compact expressions
only when aliases need resolving. Scope opens lower to ordinary entry labels;
closes lower to empty records. Simple named namespaces use `.namespace name`,
`name .namespace` or `label .namespace name`, and close with `.endnamespace` or
`.endn`. Operand-only openings create no address value and may reopen an existing
namespace. Labelled openings preserve the ordinary parent-scope address label.
Blocks and namespaces nest, and closing kinds must match. Namespace identity can
coexist with a same-named scalar. Assembly and dependency evaluation stay numeric.

Single-source sequential `.module dotted.id` / `.endmodule` regions support
`.pub`/`.priv`, private-by-default definitions and fully qualified public references
between modules. Module-local `.use target` and `.use target as alias` support
forward targets and references before imports. Explicitly ordered physical files
share one preparation session, with independent EOF and line numbering.
Visibility is inherited and restored across
blocks/namespaces. Owning module identity is separate from lexical dotted prefixes:
a child module can find a parent's public symbol, but cannot access its private
symbols. Ordinary labels on module and visibility directives retain their address
and the visibility in effect before the directive. Explicit-module sources allow
only module boundaries and `.end` outside modules.

Aliases and unused directive entries still occupy provisional ID/value slots;
this increment does not compact the final symbol table.

F8 discovers candidate modules and prepares only requested files. F9 expands
whole-line `.include` directives in those files during streaming preparation,
with nested relative includes and separate include roots. Both dependency
ordering and assembly consume packed records only. Numeric origin spans preserve
physical file/line locations through graph reordering.

Other limits remain explicit:

- no implicit file-derived modules, module metadata, dotted import aliases,
  preprocessor-generated includes, anonymous blocks or dotted
  block/namespace declarations,
  macros, conditionals, loops, structs or lists;
- no strings, general sections, relocations, relaxation or complete expression
  operator set;
- no discontiguous `.org` after output has started;
- literals are limited to the currently checked signed 32-bit subset, and source names
  cannot reuse reserved package spellings;
- maximums include 64 tokenizer tokens per line, 512 provisional source IDs,
  63-byte qualified source names, a 16 KiB preparation name arena, 4 KiB textual
  line, 256-byte packed line and 1 MiB per growing allocation;
- diagnostics are provisional and the normal native CLI is not qualified through
  this route.

The host builds the current BSP3 package capsule, stages explicit source files
and supplies an ordered-file or discovery/search-root manifest to
the native harness. Native tokenization creates the packed records; preparation
compiles expressions and binds names; assembly consumes those records without
consulting source text. This proves the execution boundary, but normal native
package loading and product integration remain future work.

## Current compact native runtime

M1 detached TABL/CSEM and VALP execution from the large legacy assembler state.
The existing text path and experimental path share the maintained interpreters.
M2 introduced lifetime-owned, right-sized storage, line-by-line preparation and
release of lexical state. Later checkpoints compiled and folded expressions,
adopted a compact runtime expression encoding, attributed preparation cost, added
indexed preparation-time binding, and measured tokenizer work. M8 retained one
generic tokenizer improvement: conditional target bytes are decoded only when the
validated branch condition is taken.

This is a controlled replacement foundation. It does not imply that every
historical native path should be wrapped or that target semantics should move out
of packages. It stays parallel to the reference only while language coverage and
product integration are incomplete.

## Measured M8 baseline

Measurements use the `expression-layout` workload with 32 blocks (323 lines) on
the reproducible 68020, AmigaOS 3.1+, 2 MiB chip-RAM guest with expansion memory
disabled. Times are host-observed guest `START` to `DONE`, include executable load,
I/O, preparation and assembly, and exclude emulator boot and host capsule creation.
They are emulator observations with 20 ms polling, not physical-machine timing.

| Quantity | m6502 | m68000 |
|---|---:|---:|
| Source bytes excluding LF | 6,418 | 7,155 |
| Packed source bytes | 4,773 | 5,477 |
| Compact expression payload | 1,508 | 1,508 |
| Retained allocation after preparation | 16,384 B | 139,264 B |
| Peak owned allocation | 86,016 B | 270,336 B |
| Tokenizer VM instructions | 28,778 | 31,306 |
| Committed tokens | 2,376 | 2,696 |
| Source-byte reads | 26,388 | 29,560 |
| Conditional branches / taken | 16,294 / 2,700 | 17,862 / 2,860 |

The release image is 20,988 bytes (`fnv1a64:5c23a01ee3b1adaa`) and its linked
reservation is 24,020 bytes. All tracked owned allocations are released after
cleanup. The m68000 peak is dominated by package-prefix relocation; the m6502 peak
includes the allocator capacity step introduced by indexed binding.

Against M7 `5459d117`, two release rounds measured 1.989/1.997 seconds versus
1.924/1.959 for m6502, and 3.888/3.880 versus 3.797/3.829 for m68000. The observed
mean reductions are about 2.6% and 1.8%. These modest results are close enough to
polling and run variation that they are not precise speed guarantees. Work counts,
packed bytes, expression payload and allocation are unchanged; the improvement
removes native decoding work from untaken VM branches.

Earlier retained results that matter to the current design:

- M2 completed mixed8 and mixed32 on both source targets in the same 2 MiB guest.
  At mixed32 it measured 1.148 seconds for m6502 and 3.624 seconds for m68000, with
  peak owned allocation 47,104 and 270,336 bytes at that checkpoint.
- Compact expressions reduced the expression payload from 3,306 to 1,508 bytes
  (54.4%) while preserving behavior.
- Indexed preparation-time binding reduced instrumented binding/record work about
  58% for m6502 and 82% for m68000. Its scratch allocation crossed a 32 KiB to
  64 KiB allocator boundary; retained memory did not grow.
- M7 release measurements improved the complete expression-layout32 case about
  11.1% for m6502 and 19.1% for m68000 versus M6. These are bounded emulator
  comparisons, not whole-product or hardware claims.

The detailed historical tables and transient receipts were removed from the live
tree; the commits preserve them.

## Telemetry and proof

Reusable macros gate allocation, phase, layout, work, clock, tokenizer opcode/pair
and nested-scope telemetry. Release builds contain no telemetry code, storage or
observer imports. MEM5 records tokenizer work in a bounded 1,756-byte schema.
Per-opcode and per-read probes perturb timing substantially; use them to compare
work counts and release builds for elapsed-time claims.

The M8 qualification includes exact output against live Rust and independent
workload bytes for both targets, clean success and rejection cleanup, and relocation
of the offset-based blocks. Forty-seven actual-native tokenizer branch cases cover
taken and untaken invalid targets, truncated operands, EOF, unknown classes and
the exact step-budget boundary. F1 qualification is recorded below. The normal
native CLI was not separately qualified end to end, and no full-language, self-host, physical-hardware, allocation-failure or
complete diagnostic-parity claim is made.

## Reproduction

Build the current assembler test executable, configure the FS-UAE environment from
the [emulator guide](../../agents/rules/fs-uae.md), then run:

```sh
python3 scripts/performance/prepared_source_native.py \
  --native-test target/debug/deps/asm-<current-test-hash> \
  --binary-source --binary-only --memory-profile 2m \
  --workload expression-layout --blocks 32 \
  --output /tmp/opforge-binary-source
```

Add `--compare-memory` for a separate instrumented accounting run. Use a fresh
output directory. To compare an older release baseline, extract that commit's
`native` tree into a temporary directory and pass `--native-source-root <root>`;
producer and consumer formats must still match. Do not interpret an old MEM4 record
with the current MEM5 reader.

Keep the existing bounds: 10 seconds after guest `START`, 60 seconds per invocation
and 150 seconds per batch. Never use the non-completing self-host case for routine
measurement or turn a timeout into a timing result. Preserve the fresh challenge,
guest completion, explicit exit, live Rust oracle and ephemeral guest-artifact
requirements in the [native parity contract](../../agents/rules/native-rust-parity-porting.md).

## F2 baseline for the current breadth increment

F1 introduced definition-order immutable constants. F2 makes the retained 6502
page-copy and 68000 register-copy cases assemble through the binary path. The
package defines which indexed register and width applies; generic native code
uses numeric shapes, name IDs and register classes. The base m6502 Rust selector
consumes the same canonical rows, including in a package-mutation test. Existing package value programs preserve
Rust label-address evaluation, unresolved wide reservation and width convergence. Other MOS
CPU variants keep their specialized selection routes; this is not their migration.

BSP3 retains the 76-byte header and uses 32-byte candidate rows. New fields hold a
base-relative exclusion-list offset and an optional table-program ID. Equal
exclusion lists share storage. Each list contains an unsigned word count followed
by operand/name-ID word pairs. Scoped register lookup is first-name-wins.

Exclusions are conservative proofs derived from canonical rejection predicates:
only actual known registers that fail a recognized class or named-range conjunct
can exclude a candidate. Unsupported forms and unknown names provide no proof;
they cannot bypass a higher-priority unsupported row. The native runtime does not
perform string or register-range parsing. Named-register projections compare IDs
and supply the canonical scalar zero.

Semantic programs can emit operand payloads rather than whole instructions.
Native now composes these with the package table program. It elides only the exact
identity table, and otherwise copies at most 24 payload bytes into the now-dead
projection buffer before table execution, avoiding input/output overlap. No additional
owned buffer or retained source-text representation is introduced; the runtime
package itself grows.

The broader limits above remain. In particular, fixed two-pass layout is not a
general relaxation/dependency solver: forward indexed references reject explicitly
rather than select a provisional short width and move later labels. F3 repairs the Rust cyclic-constant gap; both Rust and native now reject those
cycles.
The selection module is now about 940 lines. Before further operand expansion,
reassess separation of operand projection from row selection rather than keep
adding responsibilities to it.

### Validation and reproduction

F2 host coverage includes independent exact-byte routine/boundary oracles, twelve
invalid-operand oracles, canonical package mutation, numeric predicate scope/range
checks and capsule relocation/bounds checks. All 431 VM unit tests pass, as do the
focused assembler contracts, library Clippy, formatting, workflow checks and native
engineering guards.

Fresh native completion and exact live-Rust output passed for page-copy,
indexed boundaries, register-copy and register/signed-immediate boundaries. Nine
invalid-operand cases passed native rejection checks: accumulator/wrong index,
byte/word address overflow, banked register, leading-zero banked name, extended
register, wrong register class and signed upper overflow. The forward indexed
reference separately passed as an explicit native subset rejection; Rust accepts
it. These checks establish rejection behavior, not matching diagnostic wording.

All instrumented cases released their tracked allocations without profiling
errors. Final positive-case accounting on the 68020 / 2 MiB guest:

| Case | Source bytes | Packed bytes | Expression bytes | Compiles / evaluations | Retained after preparation | Peak owned |
|---|---:|---:|---:|---:|---:|---:|
| 6502 page copy | 517 | 258 | 59 | 13 / 30 | 16,896 B | 82,688 B |
| 6502 indexed boundaries | 330 | 251 | 49 | 12 / 42 | 16,640 B | 82,176 B |
| 68000 register boundaries | 238 | 130 | 10 | 3 / 6 | 131,328 B | 262,400 B |

Capsules grew from F1's **7,272 / 96,448 B** to **10,874 / 126,550 B** for
m6502 / m68000. The retained runtime prefixes are **10,124 / 121,400 B**;
lexical dictionaries and tokenizer data are released before assembly. This is a
real metadata cost of added coverage, not a memory optimization. The release
page-copy run completed in **0.340 seconds**; this is one observation, including
input/preparation/output and excluding emulator boot.

For native reproduction, use the known-good invocation environment from the
[FS-UAE guide](../../agents/rules/fs-uae.md), set `OPFORGE_FS_UAE_SMOKE=1`,
`OPFORGE_FS_UAE_MEMORY_PROFILE=2m`, `OPFORGE_FS_UAE_TIMEOUT_MS=60000`,
`OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=10000` and `OPFORGE_FS_UAE_POLL_MS=20`:

```sh
OPFORGE_COMPARE_MEMORY=1 cargo test -p asm binary_constants_page_copy_fs_uae -- --ignored --nocapture --test-threads=1
OPFORGE_COMPARE_MEMORY=1 cargo test -p asm binary_selection_indexed_fs_uae -- --ignored --nocapture --test-threads=1
OPFORGE_COMPARE_MEMORY=1 cargo test -p asm binary_selection_registers_fs_uae -- --ignored --nocapture --test-threads=1
OPFORGE_COMPARE_MEMORY=1 OPFORGE_SELECTION_REJECTION=banked_leading_zero cargo test -p asm binary_selection_rejection_fs_uae -- --ignored --nocapture --test-threads=1
```

Other rejection selectors are listed in
[binary_source_selection.rs](../../crates/opforge-asm/src/tests/binary_source_selection.rs).
Keep emulator invocations serialized and each batch under 150 seconds. Set
`OPFORGE_COMPARE_MEMORY=0` for release timing. These cases prove assembly output,
not execution of the resulting routine on its target CPU.

### Release regression comparison

One fresh matched `expression-layout32` observation per target compared F1
`e83d290c` with F2 using each revision's matching native tree and package producer:

| Source target | F1 | F2 |
|---|---:|---:|
| m6502 | 1.923 s | 1.950 s |
| m68000 | 3.806 s | 3.850 s |

Source and output digests match across revisions; each output also matches its
live Rust oracle. START-to-DONE includes input, preparation, assembly and output,
but excludes boot. These small increases are single observations with 20 ms
polling and run variation, not precise slowdown bounds or evidence of a speedup.
All runs remained within the existing 10-second guest limit on the 2 MiB profile.

The release image grew **21,176 → 21,792 B** (+616 B, 2.9%), and linked reservation
**24,200 → 24,764 B** (+564 B). F2 release image identity is
`fnv1a64:af3d0912815c5e71`; telemetry is absent. This step buys language coverage,
with the package-size cost recorded above; it is not a new optimization claim.

## F3: bit operators and absolute dependencies

F3 adds symbolic masks/configuration routines without source replay or dependency
propagation by repeated assembly passes. Production Rust was repaired first so
native comparisons use correct independent outputs, including a reversed
128-definition chain and mixed `.const`/`=` host definitions. The main native CLI
and full language migration remain outside this checkpoint.

The same 32-block expression-layout sources, package and exact outputs were
compared with frozen F2 `6decb730` on the 68020 / 2 MiB profile. Clock speed remains
uncalibrated. Release runs have telemetry off; these are single observations,
including host polling, not statistically established small regressions.

| Source target | F2 release | F3 release | Observed difference |
|---|---:|---:|---:|
| m6502 | 1.9532 s | 1.9793 s | +1.3% |
| m68000 | 3.8569 s | 3.8833 s | +0.7% |

The release image grows **21,792 → 23,300 B** (+1,508 B, 6.9%); linked reservation
is **24,764 → 26,204 B** (+1,440 B). Final release identity is
`fnv1a64:6b4a113a82e97323`. The package is unchanged. This is added language coverage
at approximately unchanged measured throughput, not a speedup claim.

Instrumented page-copy, indexed and register-boundary reruns retain exactly the
F2 owned-memory figures above. Page-copy expression evaluations fall from 30 to
27 because its three absolute constants no longer evaluate twice. New cases:

| Case | Source / packed bytes | Compiles / evaluations | Retained after preparation | Peak owned |
|---|---:|---:|---:|---:|
| Pixel mask | 643 / 362 | 19 / 28 | 16,896 B | 82,688 B |
| 128-definition chain | 2,646 / 2,366 | 132 / 136 | 20,480 B | 88,064 B |

Final-image validation also passes the control-word routine, precedence and
PC/earlier-label behavior, all 82 compact/canonical evaluator cases, and retained
F1/F2 page-copy/indexed/register routines. Native explicitly rejects ordinary and
PC-tainted cycles, missing symbols, duplicate/colliding definitions, signed32
range overflow and unsupported forward layout dependencies.

Both new cases match the repaired live Rust oracle and independent expected bytes.
Tracked memory returns to zero after cleanup, with no profiling errors. Memory
figures are tracked allocations, not total machine/process memory.

Reproduce the release comparison using each revision's matching frozen test
producer and native source tree:

```sh
python3 scripts/performance/prepared_source_native.py \
  --native-test /path/to/matching-asm-test \
  --native-source-root /path/to/matching-source-tree \
  --binary-source --binary-only --memory-profile 2m \
  --workload expression-layout --blocks 32 --output /tmp/f3-comparison
```

Use the FS-UAE environment from the [runner guide](../../agents/rules/fs-uae.md).
The existing 10-second post-start, 60-second invocation and 150-second batch
limits apply. No self-host measurement was used. Fresh proof is required when
reproducing these observations; archived logs are not a substitute.

## F4: named block scopes

F4 completes lexical binding during preparation, including declarations that
shadow outer names later in the source. Two narrow Rust reference fixes accompany
it: scalar lookup respects the nearest actual binding before pass-local scalar
markers, and unterminated blocks/namespaces produce an error at assembly end.

The m6502 copy/fold and m68000 control-word fixtures each have a flat equivalent
and independently checked output bytes. Nested checks exercise forward local
shadowing, parent lookup, absolute qualified names, case folding and ID rewriting
inside compact expressions while preserving literal operands. These prove
assembly output, not execution of the emitted routines on their target CPUs.

Separate instrumented native observations on the 68020 / 2 MiB profile:

| Case | Packed scoped / flat | Compiles / evaluations (both) | Retained after preparation (both) | Peak owned (both) |
|---|---:|---:|---:|---:|
| Copy/fold | 472 / 464 B | 24 / 41 | 16,896 B | 82,688 B |
| Control word | 307 / 299 B | 15 / 26 | 131,584 B | 262,656 B |

Scope closes account for the extra eight packed bytes in each pair. Preparation
scratch grows by 4,172 B before allocation rounding; these cases remain within
the same rounded allocation sizes. Both pairs return tracked memory to zero with
no profiling errors. These figures cover owned allocations, not total machine
memory. Provisional aliases remain allocated ID slots; no final-ID compaction is
claimed.

The initial image failed because an odd-sized embedded name table misaligned
following instructions. Explicit alignment corrects that defect; only fresh
runs of the corrected image count as native evidence.

All nine scope rejection cases complete with the expected error and allocation
cleanup, including cross-scope cycles, unclosed/unmatched scopes, duplicates,
sibling leakage, missing qualified names, close operands, and intentionally
unsupported anonymous/dotted block declarations. The 512-ID case completes in
6.356 s with telemetry enabled; 513 IDs reject. The reversed 128-definition chain
also passes. The ordinary 10-second guest and 60-second invocation bounds remain.
One unsupported-form invocation timed out; it was not counted as evidence, and
the separate fresh run completed within the original bound.

### Release comparison

Frozen F3 `92a78722` and final F4 use the same package, source and output digests
for each 323-line expression-layout workload. Telemetry is off and each native
output also matches the live Rust oracle.

| Source target | F3 | F4 | Observed difference |
|---|---:|---:|---:|
| m6502 | 1.9707 s | 2.0432 s | +3.7% |
| m68000 | 3.9041 s | 3.9760 s | +1.8% |

These are one observation per revision/target with 20 ms polling, not precise
slowdown bounds. The release image grows **23,300 → 25,240 B** (+1,940 B, 8.3%);
linked reservation grows **26,204 → 28,100 B** (+1,896 B). Release identity is
`fnv1a64:93830dd37f3e79fa`. The package and numeric assembly/dependency modules
are unchanged. This step adds scope functionality at a modest observed cost.

The same final release image also produced these single scoped/flat observations:

| Routine | Scoped | Flat |
|---|---:|---:|
| Copy/fold | 0.3749 s | 0.3752 s |
| Control word | 0.6225 s | 0.6508 s |

Each pair emits identical bytes. These small inputs and host polling do not
establish a speed difference; the sources also differ in textual name lengths,
so the comparison includes their tokenization cost rather than isolating scope
resolution.

Reproduce the before/after workload with the command in the F3 section, using
each revision's matching native source and test producer. For practical scope
cases, use the FS-UAE environment from the runner guide,
`OPFORGE_FS_UAE_MEMORY_PROFILE=2m`, `OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=10000`,
`OPFORGE_FS_UAE_TIMEOUT_MS=60000` and `OPFORGE_FS_UAE_POLL_MS=20`, then run:

```sh
cargo test -p asm binary_scopes_copy_fs_uae --lib -- --ignored --nocapture --test-threads=1
```

Replace `copy` with `copy_flat`, `control`, `control_flat` or `nested` for the
other positive cases. Set `OPFORGE_COMPARE_MEMORY=1` for separate accounting
runs; leave it unset or set it to `0` for release timing. Negative checks use
`binary_scopes_rejection_fs_uae` and `OPFORGE_SCOPE_REJECTION` with a selector
from `binary_source_scopes.rs`. Keep invocations serialized and batches below
150 seconds. Build the matching test executable before collecting comparisons;
do not rebuild or change native inputs during a measurement batch.

## F5: namespaces and canonical labels

Namespace identity is preparation-only metadata, separate from symbol value and
declaration status. Reopening an operand-named namespace reuses its qualified
names; labelled namespace forms still define their ordinary address labels. A
kind stored in an existing entry word validates mixed block/namespace closes.
No scope table, entry-size increase or new runtime record fields are needed.

Canonical labels are recognized from the column information retained by the
tokenizer. Column-one identifiers normalize to the existing binary label prefix,
including standalone labels and labels before instructions or shared directives.
An optional adjacent colon remains supported. The normalizer checks record
capacity and the 256-byte limit before inserting the prefix marker; assembly does
not consult text. Indented labels reject. A column-one package-reserved spelling
is rejected under the existing native naming limit, rather than silently emitted
as an instruction; Rust permits such label names.

The namespaced copy/fold and control-word fixtures use bare labels and match both
live Rust and independent expected bytes. A mixed case checks reopening, forward
shadowing, parent lookup, namespace/value name coexistence, distinct label/operand
names and the addresses of standalone and labelled-data forms.

Separate gated native accounting on the 68020 / 2 MiB profile:

| Case | Source / packed | Compiles / evaluations | Retained after preparation | Peak owned |
|---|---:|---:|---:|---:|
| Namespaced copy/fold | 434 / 472 B | 24 / 41 | 16,896 B | 82,688 B |
| Namespaced control word | 394 / 328 B | 15 / 26 | 131,584 B | 262,656 B |
| Mixed namespaces/labels | 402 / 334 B | 19 / 33 | 16,896 B | 82,688 B |

The practical routines retain F4's expression work and measured allocation peaks.
These fixtures differ in source formatting and scope records, so their sizes do
not isolate namespace overhead. All three free their tracked memory with no
profiling errors. Provisional namespace/alias IDs still occupy symbol slots; the
existing 512-ID and name-arena limits apply.

Fourteen native rejection checks cover label placement/reserved spellings,
namespace-only names used as values, missing/invalid/extra namespace operands,
close-kind mismatches, scope imbalance, duplicate definitions after reopening,
close operands and intentionally unsupported dotted scope names. Each validates
fresh completion, the expected error and allocation cleanup. Retained F4 nested
bindings and the optional-colon copy/fold fixture also pass on the final image.

Focused host checks pass: 23 binary-source tests and three VM-only namespace
tests. Native formatting, redundant-test checks and workflow/architecture guards
pass. Production Rust is unchanged; the broad host suite was not rerun, and the
previously recorded baseline failures are not resolved by this slice.

Use the F4 reproduction environment and deadlines with
`binary_namespaces_copy_fs_uae`, `binary_namespaces_control_fs_uae` or
`binary_namespaces_mixed_fs_uae`. For negative cases, run
`binary_namespaces_rejection_fs_uae` with `OPFORGE_NAMESPACE_REJECTION` selecting
a case from `binary_source_namespaces.rs`. Accounting uses
`OPFORGE_COMPARE_MEMORY=1`; release timing must leave it off.

### F5 release comparison

The unchanged expression-layout32 workloads use matching frozen F4 `11561735`
and final F5 producers/native trees. Package, source and output digests match
between revisions, and each output matches its live Rust oracle.

| Source target | F4 | F5 | Observed difference |
|---|---:|---:|---:|
| m6502 | 2.0624 s | 2.0764 s | +0.7% |
| m68000 | 3.9640 s | 3.9699 s | +0.1% |

These single observations differ by less than the 20 ms polling interval and do
not establish a slowdown or a speedup. The release image grows **25,240 →
25,600 B** (+360 B, 1.4%); linked reservation grows **28,100 → 28,460 B**. Release
identity is `fnv1a64:3d614b5020946b35`. Fixed preparation storage and entry sizes
are unchanged. The same 68020 / 2 MiB profile and 10-second guest, 60-second
invocation and 150-second batch limits apply; no clock calibration or self-host
claim. Reproduce using the unchanged workload command above with each revision's
matching frozen source and producer.

## F6: single-source modules and visibility

Module ownership and visibility are resolved during preparation. Three bounded
512-word side arrays record declaration ownership, reference origin and flags.
A mixed-origin flag prevents a legal local use from hiding an illegal use of the
same numeric ID from another module. Final access checks cover declared IDs and
forward aliases alike. Public definitions allow any origin; private definitions
require their owning module (global-owned labels follow Rust's global visibility).
Dotted module prefixes remain lexical parents without becoming owning modules.

The new `binary_modules.asm` owns this metadata and module lifecycle; existing
scope processing classifies directives, preserves ordinary labels and restores
visibility on scope close. The 16-byte symbol entry and runtime record formats are
unchanged. Temporary scope/module storage grows **25,172 → 28,256 B** (+3,084 B),
and is freed before assembly. There is no runtime module table or string lookup.
Provisional module/prefix IDs still consume slots under the existing 512-ID limit.

Separate gated native accounting on the 68020 / 2 MiB profile:

| Case | Source / packed | Compiles / evaluations | Retained after preparation | Peak owned |
|---|---:|---:|---:|---:|
| Module copy/fold | 709 / 585 B | 29 / 48 | 17,408 B | 83,456 B |
| Module control word | 630 / 419 B | 19 / 32 | 131,584 B | 262,656 B |
| Module visibility and labelled directives | 336 / 238 B | 12 / 21 | 16,640 B | 82,176 B |

All three match live Rust and independent expected bytes, with zero live tracked
allocation after cleanup and zero profiling errors. The routines use canonical
bare labels. These fixtures add exported aliases and expressions, so their work
counts and sizes are not an isolated measurement of module overhead.

Sixteen native rejection cases cover private and forward private access, mixed
reference origins, dotted-prefix ownership, restored visibility, labels retaining
previous visibility, duplicate/nested/unclosed modules, illegal scope placement,
open child scopes, unmatched closes, outside content, malformed names and extra
operands. Each requires fresh completion, the expected error and allocation cleanup.
Retained mixed-namespace, nested-scope and optional-colon copy/fold cases also pass
on the final image: 22 native functional checks in total.

Use the F4 reproduction environment and deadlines with
`binary_modules_copy_fs_uae`, `binary_modules_control_fs_uae` or
`binary_modules_mixed_fs_uae`. For negative cases, run
`binary_modules_rejection_fs_uae` with `OPFORGE_MODULE_REJECTION` selecting a case
from `binary_source_modules.rs`. Accounting uses `OPFORGE_COMPARE_MEMORY=1`;
release timing must leave it off.

Focused host checks pass: 26 binary-source tests and three VM-only module tests.
Rust and native formatting, changed-file redundant-test checks, workflow/CPU
boundaries and the native proof-contract guard pass. The whole-tree redundant-test
check reports the same four autofixable findings in `tkpkg_compact_table.asm` as
the frozen F5 baseline; they are outside this change. Production Rust is unchanged;
the broad host suite was not rerun and its recorded baseline failures remain.

### F6 release comparison

The unchanged expression-layout32 workloads compare frozen F5 `5067b41d` with
final F6 using their matching producers/native trees. Package, source, output,
runner and effective emulator-configuration digests match; both revisions produce
the live Rust oracle's exact bytes.

| Source target | F5 | F6 | Observed difference |
|---|---:|---:|---:|
| m6502 | 2.0953 s | 2.0797 s | −0.75% |
| m68000 | 4.0161 s | 3.9858 s | −0.75% |

These small single observations do not establish a speedup or a meaningful
regression. The release image grows **25,600 → 26,668 B** (+1,068 B, 4.2%);
linked reservation grows **28,460 → 29,496 B** (+1,036 B). Release identity is
`fnv1a64:5303e715e573341f`. The extra preparation metadata is freed before assembly;
no module metadata is retained in runtime records. The same 68020 / 2 MiB profile,
10-second guest, 60-second invocation and 150-second batch limits apply, without
clock calibration or a self-host claim. Reproduce with the unchanged workload
command above, each revision's matching producer/native tree, and memory
instrumentation disabled.

## F7: explicit source files and imports

The experimental harness accepts the current BSP3 package followed by a
big-endian file count and length-prefixed guest paths. It opens each supplied
file separately and streams lines through one frontend session. The runner binds
all file paths and contents into the fresh case challenge. The former appended
text input is replaced; no legacy input reader is retained.

The caller supplies assembly order. Native module discovery, search paths,
dependency ordering and `.include` remain deferred. Multi-file inputs require
complete explicit modules in each nonempty file; the one-file global source
form remains available. EOF resets local line numbering and file-termination
state, without discarding declarations or module identities. `.end` terminates
its own input only; this remains narrower than Rust's no-op `.end` semantics for
statements following it. The comparison fixtures use terminal `.end` consistently.

Module-local `.use target` and `.use target as alias` are resolved after every
file is prepared, so references can precede imports or target definitions.
Simple aliases are case-insensitive; imports in blocks/namespaces reject.
Reference IDs are keyed by their originating module to distinguish identical
alias spellings bound to different targets. They share interned spelling bytes
and resolve to canonical declaration IDs before assembly. Visibility checks
still apply to the resolved declaration. Alias matching uses the final spelling
component, independently of how the symbol was first interned.

`binary_imports.asm` owns import/proxy handling; `binary_scope_layout.asm` shares
preparation layout without circular module dependencies. Import scratch adds
**4,610 B**, raising scope/module/import storage from **28,256 to 32,866 B**.
Proxy IDs use the existing 512-ID budget; the final value table is not compacted.
Fixed frontend regions precede variable scope storage so 68020 signed-16-bit
address displacements remain valid. The dynamic dictionary-node base uses a
full-width offset addition.

Packed lines retain their existing local line word. A separate numeric table
uses 12 bytes per input file (record-start/end offsets and file ordinal), retained
for diagnostics and freed at cleanup. No path or source text is consulted during
assembly. Preparation errors and record-dispatch failures report hexadecimal
file ordinal/local line; global binding and pre-dispatch dependency errors still
report zero/zero rather than a misleading last-file location. Those remaining
diagnostic attribution limits are explicit future work.

The 6502 reversal/call and 68000 mask/call fixtures each span three actual files.
Every comparison obtains a live Rust file-graph oracle, checks independent
23/26-byte outputs and verifies the joined source has identical semantics. A
second case distinguishes the same alias spelling in two modules with different
forward targets, including target/alias/`as` names first interned by qualified
declarations. Canonical bare labels are used throughout.

Separate gated native accounting on the 68020 / 2 MiB profile:

| Case | Source / packed | Compiles / evaluations | Retained after preparation | Peak owned |
|---|---:|---:|---:|---:|
| Three-file reversal/calls | 451 / 311 B | 13 / 23 | 17,152 B | 82,944 B |
| Three-file masks/calls | 396 / 250 B | 8 / 15 | 131,584 B | 262,656 B |
| Two-file forward alias ownership | 287 / 191 B | 9 / 13 | 16,896 B | 82,432 B |

All release their tracked memory with zero profiling errors. Native error-location
checks require the actual `[file 00000002, line 00000003]` diagnostic both during
preparation and record dispatch. The latter deliberately exercises the existing
native signed32 range limit; Rust accepts the wider result, which is separately
asserted rather than treated as Rust error parity.

Twelve native rejection cases cover private imports, duplicate qualifiers, missing
modules (including unused imports), ambiguous qualified paths, scope restrictions,
missing members without alias fallback, duplicate modules across files, unfinished
EOF, diagnostic locations and the explicit-module requirement. Retained module
visibility, module copy/fold and mixed-namespace cases also pass: 18 native
functional checks on the final image, each with fresh protocol/exit and the
appropriate exact output or error plus cleanup checks.

Host validation passes 30 binary-source tests, four VM-only file/import tests and
45 shared-runner tests. Native/Rust formatting, changed-file redundant-test checks,
architecture and proof-contract guards pass. No broad host-suite qualification is
claimed; the recorded baseline failures remain outside this slice.

Reproduce using the earlier FS-UAE environment and deadlines with
`binary_files_copy_fs_uae`, `binary_files_control_fs_uae` and
`binary_files_aliases_fs_uae`. Set `OPFORGE_FILES_JOINED=1` for the joined copy/control
inputs. Select a negative case from `binary_source_files.rs` with
`OPFORGE_FILE_REJECTION` and run `binary_files_rejection_fs_uae`. Memory accounting
uses `OPFORGE_COMPARE_MEMORY=1`; release timing leaves it unset. All source files
are staged into the disposable guest tree and removed by the shared runner.

### F7 joined versus split comparison

The same final release image assembles each program as one joined file or three
physical files, in identical module order, with exact output equality:

| Source target | Joined | Three files | Observed difference |
|---|---:|---:|---:|
| m6502 | 0.3836 s | 0.4227 s | +39 ms (+10.2%) |
| m68000 | 0.6801 s | 0.6893 s | +9 ms (+1.4%) |

These single observations include guest file-open/read/close work; they do not
isolate filesystem cost from timing variability. The 68000 difference is below
the 20 ms polling interval. Separate instrumented runs show identical retained
memory and peak allocation for joined/split copies: **17,152 / 82,944 B** for
m6502 and **131,584 / 262,656 B** for m68000. The 12-byte versus 36-byte file-span
tables occupy the same minimum allocation size. Joined packed sources are
303/242 bytes versus split 311/250 bytes: the two additional terminal `.end`
lines lower to empty records. Expression work and final bytes are unchanged.

### F7 release comparison against F6

Frozen F6 `36bc145c` and final F7 use their matching producers/native trees on
the unchanged expression-layout32 inputs. Package, source, output, Python runner
and effective emulator settings match. F7's producer stages a separate source
file and manifest, so the measured guest work includes the new file handling.

| Source target | F6 | F7 | Observed difference |
|---|---:|---:|---:|
| m6502 | 2.0825 s | 2.1213 s | +1.9% |
| m68000 | 4.0093 s | 4.0007 s | −0.2% |

These are single observations: the 6502 case adds about 39 ms; the 68000 difference
is below the 20 ms polling interval. No general speedup is claimed. The release
image grows **26,668 → 29,144 B** (+2,476 B, 9.3%); linked reservation grows
**29,496 → 32,004 B** (+2,508 B). Final release identity is
`fnv1a64:8ea5afe318096ef9`. The same 68020 / 2 MiB profile, uncalibrated clock and
10-second guest / 60-second invocation / 150-second batch bounds apply. This is
bounded experimental-path qualification, not native CLI or self-host coverage.

## F8: numeric module graph and guest-side search

F8 adds an opt-in graph mode to the experimental native harness. The first guest
file is the entry. Candidate files can be supplied explicitly, or the guest can
scan the entry directory followed by configured roots recursively for `.asm` and
`.inc`. Duplicate discovered paths are read once. This search runs during
preparation; it does not select an instruction implementation or alter the
CPU-neutral packed format.

Preparation captures each explicit module as a packed-record offset span and each
module-level `.use` as a numeric edge. An iterative depth-first walk starts from
**all** entry-file modules, follows imports in source order, rejects missing
modules and cycles (including entry-file cycles), and emits each selected span
once after its dependencies. The harness copies only those packed spans and their
numeric file ordinals into the assembly input. Source paths and directory scratch
are released before assembly, and original source text is never consulted by the
assembly passes. Per-record diagnostic location still uses numeric file/line
metadata. A pre-dispatch graph error currently reports file/line zero.

Rust's maintained module graph was changed with this checkpoint: every entry-file
module participates in dependency traversal, and an import back to an entry-file
module is a cycle. The entry file determines search roots, not an ordering
exception. The former behavior that traversed only one selected entry module and
preloaded its siblings has been removed. Host tests cover dependency diamonds,
entry-file imports and cycles; native tests use fresh FS-UAE completion and exact
Rust output for a diamond, entry-file siblings, unused candidate, recursive
`.inc`, and overlapping roots. Missing imports and cycles have fresh nonzero-exit
proof. The F7 explicit-file path remains available for direct comparison.

At the initial F8 checkpoint, native discovery eagerly prepared every candidate.
The selective-loading follow-up below supersedes that limitation. The scanner
bounds paths at 255 bytes, recursion depth at eight and discovered files at 128;
exceeding a bound is an explicit failure, not a language rule.

On the same four-file 6502 diamond under the 2 MiB FS-UAE profile, the F7
explicit-order and F8 numeric-graph modes produced identical bytes. Both modes
use the same F8 harness image (32,364 bytes; 34,968 bytes linked reservation),
so these are mode comparisons rather than independent binary-size measurements.
Relative to the preceding F7 harness checkpoint, the F8 image adds 3,220 bytes
and linked reservation adds 2,964 bytes. With conditional memory accounting,
peak owned allocation was 82,432 bytes for explicit order and 107,520 bytes for
the graph (+25,088 bytes); retained allocation after preparation was 16,896
bytes in both. These counters cover the harness's owned allocations, not total
AmigaOS process memory. In the same instrumented run, native start-to-completion
was 0.759 and 0.759 seconds respectively. A separate uninstrumented release run
reported 0.507 and 0.252 seconds, but one short emulator sample per mode is too
variable to support a speedup claim. Discovery enumeration is not isolated by
this comparison, and the bounded workload says nothing about self-hosting time.

### Selective native discovery follow-up

The guest now reads discovered candidate files once to index leading explicit
`.module` declarations. This compact index holds folded names, offsets and file
ordinals; it does not tokenize or retain candidate bodies. The entry file is
prepared first. If numeric graph traversal reaches a missing module, preparation
looks up its unique declaring file, loads that file, and retries the traversal.
Each retry rebuilds temporary graph state. Only files in the dependency closure
reach the binary frontend. The declaration index and source paths are released
before assembly; the packed execution input still uses numeric identities and
offsets. The F7 explicit-order and F8 explicit-candidate paths remain available.

A 2 MiB mixed-source proof matched live Rust output while skipping an unrelated
invalid module, duplicate unused declarations and an unused `.include` fragment.
The existing diamond, configured-root search and fresh missing/ambiguous-module
failure cases also passed. The release harness image is 33,744 bytes and linked
reservation is 36,312 bytes: increases of 1,380 and 1,344 bytes from the initial
F8 checkpoint. This is feature cost, not a performance claim. The bounded native
completion sample for configured-root search was about 0.50 seconds; it is not a
controlled before/after timing comparison.

The index recognizes syntactically leading explicit `.module` lines. It does not
evaluate conditional or macro-generated declarations, derive implicit module
identities from filenames, or avoid lowering unused modules *within* a selected
file. These remain experimental-path differences from Rust's module loader.
Declaration count and name storage are bounded at 512 entries and 16 KiB; an
exceeded bound fails rather than silently dropping a requested module.

## F9: selected-file includes

The selected-file reader now expands literal, whole-line `.include` directives
as it tokenizes. Each active source keeps its own buffered file position; nested
fragments remain in the enclosing module and scope. The including directory is
searched before explicit include roots, which are distinct from module-discovery
roots in the experimental capsule. Missing files, cycles and depth beyond eight
fail preparation. An unused candidate with a broken include is still skipped.
Relative paths with `.` or `..` components, labelled includes and generated
include statements remain outside this checkpoint.

Packed execution records contain no source paths or pointers. A preparation-only
file stack owns handles and path strings. Compact `{start,end,file}` offset runs
identify each fragment's packed records; graph materialization intersects and
rebases these runs as it reorders modules. A fresh 68020 / 2 MiB failure proof
reported the included fragment's numeric file identity and physical line after
dependency ordering. The earlier explicit-file and discovery paths still pass
their bounded checks.

Live Rust and fresh native runs matched exact bytes for a selected module with
nested includes, a bare label and `.use` in the inner fragment, and for an include
resolved from a separate root. Missing and cyclic includes completed with nonzero
native exits. A same-source manual-inline comparison produced identical bytes;
single release guest samples were 0.512 seconds with includes and 0.507 seconds
inlined. That difference is below the useful timing resolution here and does not
establish a speed effect. The release image is 36,316 bytes with 41,252 linked
reserved bytes, up 2,572 and 4,940 bytes from the selective F8 checkpoint. The
extra nine-slot I/O scratch is preparation-only; retained and peak allocation
were not separately measured for F9. This remains a bounded feature proof, not
full native module/include parity or a self-host performance claim.

## Selected modules within candidate files

Discovery now reloads a candidate file for each newly requested module and lowers
only that module's lines into packed records. The declaration index still selects
files by folded module name; a preparation-only line filter selects the requested
body. The graph retains separate offset spans for multiple loads of the same
physical file, while provenance still identifies the original file and line.
Entry-file modules continue to follow declaration/dependency order. Include
expansion still covers the whole selected file before module extraction, matching
Rust's current preprocessing behavior: a missing include in an unused sibling of
a selected file remains an error. Unused candidate files are indexed but not lowered.
The Rust graph loader now rejects duplicate declarations of the requested module
within one selected candidate file; it previously extracted the first silently.

Fresh 68020 / 2 MiB native runs matched live Rust bytes for one selected module
beside an invalid unused sibling, and for two modules selected from the same file
with an invalid sibling between them and mixed-case spelling. The sibling cases
include both dotted and ordinary invalid source lines. The existing nested
include and explicit-order diamond cases passed, as did missing/ambiguous-module
failure cases and selected-file missing-include and duplicate-module failures.
The release harness image was 36,984 bytes; linked reservation was 41,720 bytes.
Those are increases of 668 and 468 bytes from F9. The two positive native runs
completed in about 0.52 and 0.51 seconds.
These are bounded observations, not a controlled speed comparison. Peak and
retained owned memory were not separately measured. At that checkpoint, the
implementation still required explicit module declarations in discovered
candidates and did not prune code or data within a selected module.

## Experimental native block reachability

The dependency-ordered native graph now marks whole named `.block` spans before
its two assembly passes. Blocks in the entry file are roots. References from
retained records can pull in imported blocks, including references to labels
inside a block and references chained through another retained block. A
standalone label immediately before a block is part of that block's selectable
span. Unreachable imported block records receive a compact omit flag; constant
indexing, layout and emission skip them. Other records remain in order, so
fallthrough inside a retained block is never pruned by label reachability.

This was initially the bounded single-PC path. Selection uses entry-file roots
and numeric references. A bounded native
`.use dep (entry, helper)` form now makes selected unqualified names available
without making their blocks output roots; `as alias` enables qualified access
instead. Binding validates every selected name even if no code references it.
Repeated names within a list are deduplicated. Per-item aliases and wildcard
selection remain unsupported here. Binding still validates unreachable source
during preparation, and the declaration scan is bounded but
not yet optimized for large block graphs. These native smoke cases demonstrate
selection behavior, not same-source byte parity with Rust's mapped-section
linker. A controlled Rust/native parity comparison needs section mapping in
the native path.

Fresh 68020 / 2 MiB FS-UAE completion produced the expected bytes for a
qualified reference to an internal label with an unused sibling block, a
transitive block reference, and a referenced label immediately before a
block. An unreferenced entry-file block also stayed live while an unused
imported sibling was omitted. The full-language native CLI and self-host path
were not exercised by these cases.

The selective-import follow-up passed fresh 68020 / 2 MiB cases for an unused
selection, an unqualified reference, a qualified alias reference and a local
declaration shadowing the import. Missing and private selected names produced
fresh expected failures. The pre-existing native block reachability case still
passed. The release harness image was 39,152 bytes and linked reservation was
43,764 bytes in the final focused run; guest assembly took about 0.50 seconds
for the local-shadow case. These are resource observations, not a controlled
performance comparison or mapped-section parity claim.

The multi-name follow-up retains one import item per `.use` and stores selected
numeric IDs in a separate linked list. Its fresh 68020 / 2 MiB cases covered
one referenced name with another unused, two referenced names, an aliased
qualified reference, a repeated name, and a missing second name. The prior
single-name cases passed again. The release harness image was 39,332 bytes
and linked reservation was 43,944 bytes, each 180 bytes above the previous
selective-import checkpoint. This size observation is not a speed comparison.

Ordered packed-record materialization and physical-origin remapping now live in
`binary_ordered_records`, behind a caller-owned frame and block descriptors;
the harness retains file I/O and orchestration. No packed format or output
behavior changed. Fresh 2 MiB native checks retained the explicit-order versus
numeric-graph output comparison, included-file diagnostic origin, and reachable
block output. The release harness image grew 39,332 → 39,356 bytes, and linked
reservation grew 43,944 → 44,052 bytes. One graph timing differed substantially
between runs; this refactor does not establish a performance change.

## First compact CLI checkpoint

The shared compact engine now has two entry modules: the original manifest-backed
test harness and `opforge_compact`, a separate Amiga Shell executable. The first
CLI checkpoint took `PACKAGE.bsp3 SOURCE.asm OUTPUT.bin` as three positional
paths, skipped the test manifest, and processed one source file. BSP3 still
comes from Rust's `prepare_package`; this checkpoint does not solve native
canonical package loading or full product CLI integration. The intentionally
small argument parser rejects quoted paths, spaces, missing or unrecognized
arguments, and paths over 255 bytes. The compact language subset still applies.

Fresh 68020 / 2 MiB FS-UAE execution produced Rust-identical bytes for a small
6502 source with a label, two instructions and shared data emission. A separate
unsupported-directive invocation completed with exit code 20 and the expected
diagnostic. The captured compact Hunk's linked reservation was below 2 MiB. The
manifest harness still completed a three-file copy/reversal comparison after
engine extraction. These are functional and resource checks, not a native
self-hosting or performance claim.

The next CLI slice exposes existing native dependency discovery and selected-file
includes as `opforge_compact PACKAGE.bsp3 ENTRY.asm OUTPUT.bin [-M DIR] [-I DIR]`.
The entry path anchors the first search directory (the current directory for
a bare filename); it does not force output order. `-M` roots are scanned
recursively and `-I` roots resolve selected-file includes after the including
file's directory. Search-root mode requires the
supported module syntax. With no roots the original direct single-source mode
remains available. The parser accepts up to eight module and sixteen include
roots, all unquoted paths of at most 255 bytes. Preparation-only root planning
lives in `binary_input_plan`; discovered paths and include-root copies are
released before the assembly passes. The CLI's bounded argument buffers remain
in its linked BSS until exit.

Fresh 68020 / 2 MiB FS-UAE cases assembled a four-module dependency graph, a
selected module whose include resolved through `-I`, and a bare entry filename
searching the current directory. All were byte-identical to the live Rust
oracles. The compact Hunk linked reservation was 52,380 bytes in the final
focused case. This is functional breadth, not a performance claim; canonical
package conversion remains host-side.

## First bounded section placement

The compact CLI now lowers one named concrete section, one same-name logical
section from a discovered module, one literal `.region`, and `.place SECTION in
REGION` into numeric packed controls during source preparation. The runtime
scans the region once, then applies those controls during both assembly passes.
It bounds emission to the placed region and rejects `.org` in this sectioned
mode. The source text is not consulted after preparation. The logical section
can be discovered after the root file's `.place` in preparation order; ordered
execution still emits the dependency before the importer.

A focused two-file 6502 case selects one imported `.block`, omits its unused
sibling, and emits a root `.word` reference. Fresh native FS-UAE runs under
the 68020 / 2 MiB profile produced the live Rust bytes `11 00 10`, and an
unchanged search-root control also passed. The compact Hunk linked reservation
was 54,196 bytes; guest command time was about 0.51 seconds in each case.
Earlier 2 MiB runs timed out before a guest start marker. The failure did not
recur in these bounded reruns, and its host/emulator cause remains unknown.
The reruns used `OPFORGE_FS_UAE_MEMORY_PROFILE=2m` and
`OPFORGE_FS_UAE_TIMEOUT_MS=60000` with the guide's local FS-UAE settings and
the `compact_cli_search_roots_fs_uae` and
`compact_cli_single_mapped_section_fs_uae` focused tests.
These are functional and resource observations, not a speedup claim. At this
checkpoint, explicit maps, multiple sections, discontiguous placement,
expressions in region bounds, and broader layout syntax were outside the path.

## First explicit imported section map

The compact CLI now accepts one `.use dep (entry) as d map { code -> app_code }`
for a differently named logical and concrete section. Numeric preparation
records the mapped names and their module owners; the assembly passes consume
the packed section controls and source records. This checkpoint retains the
single region, single contiguous placement and selected-block limits. The
mapped concrete section must be empty. Rust emits concrete-section statements
before mapped logical content, while the native dependency-ordered pass would
emit them afterward; native therefore rejects a nonempty mapped concrete body
until section-local ordering is implemented. A focused Rust case confirms the
ordering difference (`22 11`), and a fresh native rejection test confirms the
guard instead of accepting incorrect bytes.

With an empty concrete section and an outside-section constant reference to
`d.entry`, a fresh 68020 / 2 MiB FS-UAE compact CLI run matched the live Rust
byte `11`. The unchanged same-name section case also matched its Rust oracle,
`11 00 10`. The release Hunk in these runs was 43,224 bytes with 54,592 bytes
linked reservation. The prior checkpoint recorded 54,196 bytes reservation,
so this slice adds 396 bytes (0.73%). The mapped case's guest command time was
0.503 seconds; the unchanged control was 0.254 seconds in another fresh run.
These single-run times are recorded for visibility but do not support a speed
claim: no baseline can run the new mapped input, and emulator timing varied
substantially even on the unchanged case. The previous startup timeout did not
recur in these focused runs. Broader mapped-section ordering and multiple maps
remain unsupported.

## Concrete content before mapped import

For the same one-map, one-region subset, the native assembler now makes two
sweeps over the already packed records in each assembly pass: it assembles the
mapped concrete section first, then the remaining records including the mapped
logical section. This gives both sections their Rust-relative addresses and
lets the existing output writer emit one contiguous placed section. The packed
source is neither copied nor reparsed as text. Same-name sections retain their
single sweep. Imported named-block reachability is still determined before
assembly, so this ordering change does not make selected but unreferenced
blocks output roots.

The focused 6502 case has root bytes and a `.word d.entry`, imported unowned
bytes before and after `entry`, and an unused imported block. Fresh Rust and
68020 / 2 MiB FS-UAE runs agree on `22 05 10 33 b0 11 b1`; the `99` from the
unused block is absent. The earlier empty-map and same-name cases passed again.
No startup timeout recurred. The release Hunk is 43,396 bytes with 54,764 bytes
linked reservation, each **172 bytes** above the previous checkpoint (0.32%
reservation growth). Guest command times were 0.519 seconds for the expanded
case, 0.516 for the empty map, and 0.505 for the same-name control. These are
single-run observations, not a measured speedup or proof of no regression;
explicit mapping now scans records twice per assembly pass. Multiple maps,
multiple placements, discontiguous output and broader layout syntax remain
outside this bounded implementation.

## Two-map placement boundary

The next breadth case uses two distinct imported logical sections, each mapped
to its own concrete section. Rust's existing `.place` pass reserves only the
concrete sizes; reachable imported bytes are appended later. If both targets
are placed consecutively in one region, the first mapped addition overlaps the
second section's assigned address. The Rust reference now rejects this case
before appending mapped bytes to sections or the output image, instead of returning a
conflicting address-to-byte result. Two adjacent, separately bounded regions
provide a valid contiguous case: its address-ordered Rust image is
`a0 04 10 b0 10 a1 09 10 c0 20`, with unused imported blocks omitted.

This checkpoint is a Rust correctness repair and reference case. It does not
change the native Hunk, memory reservation or guest execution time. Native
support for two imported maps still requires indexed section state and an
output schedule; merely accepting a second `.use ... map` would leave the
current scalar runtime with ambiguous addresses. Same-region
placement after mapped growth needs a separate layout-convergence design.

## Two concrete placements in the compact CLI

The native preparation path now lowers two distinct concrete sections, two
literal regions and their placements to numeric controls. The execution path
tracks each section's bounds and completed PC separately. It accepts the second
section only when its region begins exactly where the first section's output
ends, so the existing flat writer can emit one contiguous image. This is a
layout foundation for multiple maps, not support for a second imported map.

A single-file 6502 case places `app_a` at `$1000` and `app_b` at `$1003`, with
the first section referring forward to `b_entry` in the second. Fresh 68020 /
2 MiB FS-UAE execution matched the live Rust bytes `a0 03 10 b0 b1`.
The earlier same-name section and explicit mapped-body cases also passed in
fresh runs. Moving the second region one byte ahead produced a fresh native
failure with the required diagnostic, confirming the flat-output boundary.
The compact Hunk is 44,084 bytes with 55,468 bytes linked
reservation, up 688 and 704 bytes respectively from the preceding native
checkpoint. The three guest command times observed across these focused runs
were 0.254–0.513 seconds. They are single-run observations, not a speed claim;
the prior native path cannot assemble the new input. No peak owned-memory
measurement was taken for this slice.

The two-section state is a bounded bridge. Extending beyond two sections should
grow the indexed slot and map tables rather than adding a third scalar case.
Noncontiguous placement and same-region placement after mapped growth still
need separate layout and output decisions.

## Two imported maps in adjacent regions

Preparation now retains two imported map identities in an indexed table and
lowers each logical and concrete section to a distinct numeric control. The
native assembly passes schedule concrete A, logical A, concrete B, logical B,
then outside-section controls. Section bounds and completed PCs use two indexed
runtime slots. This keeps concrete content before its own mapped import without
moving the first imported section behind the second concrete section. The
selected-block graph still omits unreachable imported blocks.

An entry source and two imported files form a 6502 case with two forward words,
unowned logical prefix bytes and unreachable siblings. A fresh 68020 / 2 MiB
FS-UAE run matched the live Rust address-ordered bytes
`a0 04 10 b0 10 a1 09 10 c0 20`. The previous one-map mapped-body and
two-concrete-section cases also passed. A second map targeting the same region
was rejected with fresh guest completion and the expected diagnostic; Rust
rejects that overlap before emitting an image.

The compact Hunk is 44,660 bytes with 56,048 bytes linked reservation, an
increase of 576 and 580 bytes respectively over the preceding native checkpoint.
The two-map guest command took 0.516 and 0.780 seconds in two fresh runs.
The unchanged one-map and two-concrete controls took 0.507 and 0.260 seconds
in their own single runs. These are functional observations, not a speed
comparison for the new input. The two-map mode makes five full packed-record
sweeps per pass; the one-map mode makes two. A prepared span schedule would avoid
that cost on larger graphs; this case does not establish its value. Peak
owned memory was not measured for this slice.

The accepted subset still requires exactly two distinct concrete targets in
adjacent literal regions, one mapped logical section per target, source names
outside package-reserved spellings, and flat contiguous output. General section
ordering, multiple targets per logical section, sparse output, and same-region
repacking after mapped growth remain separate work.

## Bounded two-map scaling baseline

`two_map_measurement_rust_oracle` and the opt-in
`two_map_measurement_fs_uae` test generate the same three-file workload for
`m6502` and `m68000`: two adjacent imported section maps, 64 data/expression
items in each selected block, unowned prefix bytes, forward references, and an
unreachable sibling block. This is a generic directive workload; it does not
measure CPU instruction encoding. Each live Rust image is 522 contiguous bytes,
and each fresh native run matched it exactly under the 68020 / 2 MiB FS-UAE
profile. Each source set has 302 lines and about 4.7 KiB of text. Native
telemetry reports 300 tokenized lines and 5,219 packed-source bytes. The
five-sweep schedule over two passes therefore implies about 3,000 packed-record
inspections for this case; that work count is derived from the schedule and
tokenized-line count, not a direct record-visit counter.

| Package | Release compact CLI command time | Instrumented preparation | Instrumented assembly | Tokenization within preparation | Peak owned memory |
|---|---:|---:|---:|---:|---:|
| m6502 | 2.03 s | 3.40 s | 0.30 s | 2.20 s | 291,072 B |
| m68000 | 2.29 s | 3.62 s | 0.32 s | 2.18 s | 405,760 B |

The release CLI Hunk was 44,660 bytes with 56,048 bytes linked reservation for
both packages. Instrumentation uses a different harness and executable; its
phase times must not be added to or compared numerically with release command
times. The source/package file preparation on the host and emulator startup are
outside guest command time. These are individual runs, not distributional
latency estimates or an identical-input before/after speed comparison: the
previous two-map runtime could not assemble this input. The packed records are
slightly larger than the input text in this case, which matters for the future
compact-format goal but does not establish a memory regression against a
functionally equivalent text path.

The entire instrumented assembly phase is much smaller than tokenization, so
even eliminating all record-sweep cost would save less than the measured
assembly phase in this workload. Defer a prepared span index until a larger
bounded input or broader feature mix shows assembly scans becoming material.
Keep this exact-output case as a baseline and inspect tokenization before
investing in scan scheduling. Reproduce the native runs with the configured
FS-UAE environment from the [FS-UAE guide](../../agents/rules/fs-uae.md),
`OPFORGE_FS_UAE_MEMORY_PROFILE=2m`, `OPFORGE_MEASURE_CPU=m6502` or `m68000`,
and `cargo test -p asm two_map_measurement_fs_uae -- --ignored --nocapture --test-threads=1`.
Set `OPFORGE_COMPARE_MEMORY=1` for a separate instrumented run; leave it unset
for release timing.
