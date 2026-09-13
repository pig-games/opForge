# String-free assembly replay experiment

Status: experimental native binary-source path implemented and measured on both
complete mixed8 workloads. The normal native CLI remains the reference.
Next agreed work: [compact native runtime, M1 then M2](#next-implementation-compact-native-runtime).
The active
[AGENTS.md](../../AGENTS.md) and [workflow](../workflow/README.md) remain binding.

This plan uses the local [plan-authoring skill](../../skills/opforge-plan-authoring/SKILL.md).

## Hypothesis and contract

Resolve source and package names during initial preparation, then perform repeated
assembly work using compact identities and structured values. The ideal is zero
string lookup after preparation and little retained original text. Mnemonic IDs
identify normalized instructions, not spellings or machine opcodes. All aliases of
one instruction share an ID; meaningful size/form qualifiers remain separate
structured fields. Normalization is package-owned and runs once during preparation. Package semantics remain authoritative;
generic directives remain shared core behavior.

- Intern names on first use, including forward references. Name identity, scoped
  symbol binding and a symbol's current value are separate responsibilities.
- Retain immutable expression structure; recompute values and layout when needed.
  Do not freeze state-dependent operand forms, relaxation or emitted bytes.
- Carry numeric references across VM and package boundaries. Recreating strings
  to call the old interfaces does not satisfy the experiment.
- Release preparation dictionaries and ASTs after binding. Keep compact locations
  for diagnostics; measure any retained text separately. No new disk source format.
- Derived package state owns its lifetime. Reload/rebinding cannot reuse references
  from an earlier preparation. Before 1.0 only the latest affected contract survives.

## S1: bounded executable Rust laboratory

Create an explicitly experimental source path alongside the current assembler.
It must complete small sources with labels, forward references, numeric expressions,
shared `.byte`/`.word` data and package-defined operandless instructions, across at
least two assembly families. Reject unsupported syntax explicitly. This scope
tests representation and lifetime, not representative instruction-selection cost.
The first path accepts only canonical `implied` instruction entries, flat ASCII
symbol names, unsigned byte/word results, literals and unary/binary `+`/`-`.
Instruction-only lines follow the compatibility parser's indented syntax. Other
forms, scopes, macros, CPU changes, relocations and full diagnostic parity remain
outside this slice. A different package form is unsupported, never guessed.

Prepare only needed canonical encoding programs into a byte arena with numeric
offsets. Drop source text, parsed ASTs, binding dictionaries and the original runtime
model before replay in a correctness test. Keep values separate from stored source
records; replay the same prepared input at different origins. Preparation may inspect
a fixed instruction's package program for its size, but replay executes that program;
the prepared source must not become a cache of final instruction bytes.

Use the ordinary Rust assembler as the complete-case output oracle. Independently
test package binding against existing lookup/encoding, including changing operand
bytes, case normalization, owner precedence, missing bindings, VM errors and budgets.
Only immutable package resolution is prepared: the general dynamic candidate route
and effect/fixup handling are not replaced by this initial directory interface.

Measurement: separate preparation from replay and include preparation in totals.
Use compile-time-gated macros over existing telemetry for parses, name lookups and
numeric accesses. Report source bytes, retained records/programs, symbol/layout
storage and exclusions; Rust object sizes are not a native layout or peak-RAM proof.
Keep benchmark execution below 30 seconds, with no self-host run. Compare identical
inputs and outputs; a microbenchmark or fewer lookups alone is not product success.

Done means a reviewable running laboratory, focused correctness checks, bounded
measurements and an explicit coverage/remaining-cost report. It does not mean the
normal CLI or native assembler has adopted the representation.

## S1M: native-oriented measurement and layout

Before adding semantic coverage, measure the existing native assembler on the same
small complete S1 cases, both with and without telemetry. Separate guest time from
emulator boot/harness time and name the actual emulator CPU and memory configuration.
Keep the existing fresh live-oracle proof contract and bounded runs. These are
baseline measurements until a native prepared-source implementation exists.

Split ordinary Rust package bootstrap, parse/layout and final output timings;
compare with prepared bootstrap, parse/bind and complete two-pass replay. Report
five-sample medians with ordering alternated, without telemetry. Bootstrap paths
and semantic coverage differ; replay ratios are not CLI or native speedups.

Probe an owned raw byte block with length-prefixed source/expression records,
explicit fields and borrowed Rust readers. Vary byte/word mnemonic identities and
record alignment independently. Compact constants and symbol IDs should avoid the
padding and broad integer widths of Rust enums. Count metadata and padding, and
retain source locations for diagnostics. Report conversion cost separately: packing
an already-built S1 object does not demonstrate a native first-pass implementation
or its peak memory. Time decoding separately from validation and assembly.

Choose token width from the package-defined normalized dictionary and reserved-tag
budget, not hardcoded CPU names or machine opcodes. A byte may suffice for a package
with at most 256 normalized identities. Aliases share an identity and meaningful
qualifiers remain separate fields. Native execution-platform alignment is independent of source target CPU
and emitted data byte order. For Amiga execution, aligned words may be cheaper than
maximum byte packing. Performance takes precedence over minimizing input bytes.

The next inspectable decision is whether to implement a small native reader and
numeric lookup/replay boundary. Do not expand the Rust-only language subset first.
No production format or runtime-package contract is adopted by this probe.

## Next slice: bounded native confirmation

The NOP-only S1 case is a correctness smoke, not the main performance workload.
Use the existing `vm_efficiency.workload` mixed case as the default: eight complete blocks,
40 instructions, 67 source lines, varied immediate values, register operands,
absolute memory stores, forward conditional branches, labels and address-valued
data expressions. Run m6502 and m68000 independently against their exact live
Rust oracle and independent bytes. The S1 packed prototype currently rejects these
operand forms. Extend the native candidate to this complete workload before using
it to claim a performance improvement; do not reduce the benchmark to what the
prototype happens to support.

This remains a focused instruction-selection workload, not a miniature self-host
or representative whole-project corpus. It omits macro/module expansion, include
I/O, backward branches and convergence-sensitive relaxation. Those are explicit
later workloads when their costs matter. Keep the current small case for isolating
record/lookup regressions, with the explicit `--workload replay-smoke` option.

The agreed first native component is numeric package-program binding. Carry
existing package identities through selected semantic execution and reuse immutable
program resolution, while projecting inputs and evaluating branches/fixups afresh.
Do not claim this replaces textual source processing. Bound metadata independently
of statement count and invalidate it on package and effective pipeline changes.
Then lower source records to native in a later inspectable slice. Compare complete small cases under the existing fresh native proof contract,
with a 60-second invocation and 150-second batch ceiling. Account for executable
size, preparation peak, retained RAM and total time. No long-running self-host test.
Delete superseded responsibilities on integration instead of retaining permanent
parallel pipelines. Review before expanding into a general native migration.

## Deferred semantic expansion: value-dependent instruction work

Use S1 evidence to select one coherent operand/selector preparation boundary.
Preserve package-defined choice rules and bind expression symbols by identity;
reevaluate values, candidate eligibility and relaxation without spelling lookup.
Include scope/macro-instance identity and changing CPU/dialect state when expanding
coverage. Do not install a generic cache keyed only by source line or mnemonic.
Connect the existing portable expression machinery to numeric symbol references;
do not grow the laboratory's small expression evaluator into another complete
expression implementation. Choose a compact storage layout from measured costs,
not by translating Rust enum layouts directly into native records.

## Decision criteria

Continue when a complete path demonstrably replays without source/package spelling
lookup and its cost/representation supports the next extension. Revise or stop if
it depends on string reconstruction, freezes state-dependent decisions, duplicates
CPU semantics, or merely layers new retained storage over old data. Do not infer
68020 feasibility from host timings or from operandless instruction coverage.

## S1 observations

The [source laboratory](../../crates/opforge-asm/src/prepared_source_experiment.rs)
stores numeric statement/symbol references, flat postfix expressions and an owned
[encoding directory](../../crates/opforge-vm/src/prepared_encoding.rs). Replays
allocate fresh symbol values and a reusable expression stack. No original source,
AST, name dictionary or original package model is retained by the frozen object.
The lifetime test drops all those inputs before layout/emission. Errors retain a
line and numeric identity; recovering original diagnostic spellings is not solved.

The instrumented six-record witness performs six source parses, four statement-name
lookups, five symbol-name lookups and five package-name lookups during preparation.
After dropping those inputs, its two assembly passes perform 12 record visits,
six expression operations, three symbol-ID loads and four encoding-program calls,
with **zero source parses or source/symbol/package name lookups** and no counter
overflow. Separately, eight ordinary encoding requests perform 40 package-name
lookups versus five once during binding and zero for eight prepared requests with
changing operand values. Counters use compile-time-gated reusable macros; the
disabled-macro test verifies argument expressions are not evaluated.

Complete-case Rust oracle comparisons cover 6502 and 68000 instructions/data at two
origins, plus little/big-endian shared data. The 8085 package does not use an
`implied` entry for NOP: that instruction route is unsupported in S1. Do not infer
that an arbitrary fixed-byte program accepts zero source operands. Extending the
package-defined form-selection boundary belongs to the deferred semantic expansion, without CPU-specific cases
in the source laboratory. The original CLI and native paths do not use this prototype.

### Reference correction exposed by the experiment

The original forward `.byte end-start` comparison exposed a Rust layout error.
Before `end` was defined, its provisional value could make the subtraction wrap;
the truncation-warning return then omitted the byte's layout width. Final emission
used the wrong forward value before updating the label later in that same pass.
Shared byte emission now preserves width for unstable expressions during layout
and checks the resolved value for truncation. Independent regression tests cover
multiple forward byte expressions on three families and preserve the warning for
a genuinely out-of-range constant. The experiment fixture was not weakened to
avoid this discrepancy.

### Bounded host measurements

The current uninstrumented benchmark takes five independent samples per case,
alternates path order, and reports medians. Each prepared replay sample averages
128 executions, with output passed through `black_box`; exact ordinary-output
comparison happens outside that timing. Registry construction is included in each
setup. Both use all four default family stacks, and the benchmark asserts their
canonical generated package equals the native package byte-for-byte.

On arm64, Rust 1.95.0, opt-level-1 test build, the combined focused checks and both benchmarks finished in
8.08 seconds (compilation excluded):

| Target / blocks | Ordinary setup | Parse/layout | Final/output | Ordinary total | Prepared setup | Parse/bind | Two-pass replay | Prepared total |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| m6502 / 8 | 129.564 ms | 0.361 ms | 11.014 ms | 140.504 ms | 128.621 ms | 0.049 ms | 1.66 us | 128.671 ms |
| m68000 / 8 | 126.737 ms | 0.357 ms | 6.628 ms | 133.698 ms | 126.888 ms | 0.052 ms | 1.72 us | 126.949 ms |
| m6502 / 32 | 126.124 ms | 1.060 ms | 11.919 ms | 139.380 ms | 126.084 ms | 0.139 ms | 5.98 us | 126.225 ms |
| m68000 / 32 | 127.882 ms | 1.098 ms | 7.222 ms | 136.809 ms | 126.593 ms | 0.139 ms | 6.68 us | 126.738 ms |

Each total is the median of per-sample sums, so it need not equal the sum of column
medians. Ordinary parse/layout includes source-line allocation and pass 1;
final/output includes pass 2, the disabled listing sink and addressed-image extraction.
Runtime trace collection is disabled. Prepared replay does both layout and emission
and allocates fresh values/output. The complete ordinary implementation has broader
responsibilities; this measures the restricted S1 laboratory, not equivalent general
assembler implementations. Final/output cost changed appreciably between successive bounded runs; do not fit
a per-line cost or claim a statistically stable speed ratio from these samples.

The setup costs dominate complete runs. Rust microsecond replay results are neither
native timings nor evidence of 68020 feasibility. No peak RAM was measured. For
32 blocks (64 instructions, 192 source records), the typed S1 frozen object still
costs 9,131/9,133 bytes versus 2,062 bytes of input; workspace payload is 536 bytes.
The 8-block objects cost 2,411/2,413 bytes versus 488 input bytes, with 152 bytes of
workspace. Allocator overhead, outputs, stacks and preparation peak are excluded.

The package's effective family/CPU/dialect spelling union contains 56 names for
m6502 and 426 for m68000. **These are not normalized instruction counts and do not
determine token width.** Aliases of one instruction must share a code; meaningful
size/form qualifiers are separate fields. The package-owned normalization boundary
must supply that mapping before choosing a minimum width. The current S1 workload
has only NOP, so the probe compares storage widths without claiming to normalize
the complete instruction set. Do not allocate a distinct identity per spelling.
The existing m68k package selector programs already own spelling aliases such as
`BHS`/`BCC` and `DBRA`/`DBF`, and branch-short qualifier rules. Consume those rules
at preparation; do not reconstruct an alias table in the generic packed reader.
The next normalization witness must prove two aliases yield the same instruction
identity while preserving meaningful qualifiers and exact output.

Reproduce timing separately from counters:

```sh
cargo test -p asm --lib prepared_source_experiment_benchmark --locked -- --ignored --nocapture --test-threads=1
cargo test -p asm --lib --features vm/prepared-telemetry --locked -- prepared_source_experiment data_layout_tests
cargo test -p vm --lib prepared_encoding --features prepared-telemetry --locked
```

### Review decision

String-free execution and releasing textual preparation state are viable for this
subset. Storage is not yet compact: the 32-block representation is roughly 4.4 times
its input text. Do not port these Rust enum layouts directly to native. Before
expanding the native implementation, define packed records/expressions and account
for preparation peak. S1M now precedes semantic expansion: measure the native baseline and compare explicit
packed layouts, then choose a native implementation boundary. A later semantic
extension should carry IDs into value-dependent package selection without growing
a second general expression evaluator.


## S1M measurements and native decision

### Current native baseline: mixed instructions

The default is now eight complete blocks from the existing
[VM-efficiency workload](../../scripts/performance/vm_efficiency.py): 40 instructions,
67 lines, and varied immediates, register operands, absolute stores, forward branches,
labels and address-valued data. This is a selection/lookup workload, not a general
assembler corpus. Both sources use origin $1000 and the same current 368,635-byte
canonical package (SHA-256 `ae77de3ff1a7966af7f33f89b89062ef4aa5bdb0ce209fedee2e6b3e44826e4a`).

| Source target | Source | Exact output | Guest START–DONE, telemetry off | Whole invocation |
|---|---:|---:|---:|---:|
| m6502 | 688 B | 104 B | 4.8044 s | 27.56 s |
| m68000 | 865 B | 144 B | 6.8635 s | 28.84 s |

These are single observations, not stable speed ratios. START–DONE is host-observed,
includes input/package/assembly/output and excludes emulator boot. The uninstrumented
native executable is unchanged: 563,768 bytes (`fnv1a64:106b19ba51b9effc`). Both actual
cases passed Level D: fresh challenge, guest start/completion, explicit zero exit and
byte-for-byte equality with the live in-memory Rust oracle. The wrapper additionally
checks the receipt against independent expected bytes from the shared workload.
The actual emulator remains A4000/68040 with 2 MiB chip, 8 MiB fast and 64 MiB
Zorro III; the native executable build target is 68020. These timings do not prove
68020/2 MiB feasibility.

Separate instrumented runs also passed fresh Level D proof, with no counter overflow.
Their internal 50 Hz totals were 200 ticks (4.00 s) and 310 ticks (6.20 s), while
host START–DONE intervals were 4.7446/6.9579 seconds. These boundaries differ; single
observations do not measure telemetry overhead. Instrumented image size remains
567,776 bytes.

| Work counter, eight mixed blocks | m6502 | m68000 |
|---|---:|---:|
| Statement visits / statements | 198 / 66 | 198 / 66 |
| Encoding service calls | 120 | 120 |
| Encoder candidate visits | 0 | 23,192 |
| Selector candidate visits | 120 | 360 |
| Compact-path lookups | 120 | 24 |
| Compact-path strings examined | 190,200 | 38,040 |
| TKVM / PRVM / EXVM / ExprVM opcodes | 2,170 / 2,121 / 208 / 232 | 1,978 / 1,577 / 248 / 248 |

The encoder-candidate counter increments inside package encoding/semantic program
search loops; it does not count successfully executed instructions or unique
programs. Zero on the compact route is not zero selection work. Likewise compact
string counters omit other routes' string processing. Both cases spend most of
their internal measured time in the three assembly passes: pass-one/layout/final
are 66/65/64 ticks for m6502 and 102/102/101 for m68000; package setup is four ticks
each. This is materially different from the setup-dominated Rust smoke.

The mixed case exposes repeated package selection that source packing alone may
not remove. The next candidate must bind package-normalized instruction identity
and prepare relevant selection state without freezing value-dependent decisions;
compare these counters as well as full output and time. Do not claim eliminating
compact lookups eliminates every name lookup or all candidate discovery.

The first 32-block probe (160 instructions) completed on m6502 in 16.7622 seconds
START–DONE, but m68000 hit the runner's 35-second completion timeout. The latter
has no valid timing or parity result. The default was reduced to eight **complete**
blocks, preserving every block's forward labels and data. The timeout was not
extended; this is a smaller measurement case, not a fix for the larger failure.

The previous NOP-only S1 baseline remains available as `--workload replay-smoke`
for correctness/localization. It took about one second per target and does not
exercise operand selection. Its raw-record/decoder results below remain smoke
results; the packed prototype does not yet support the mixed workload.

The [bounded native runner](../../scripts/performance/prepared_source_native.py)
records hashes and configuration and caps each invocation at 60 seconds, the batch
at 150 seconds, and guest completion waits at 35 seconds. A failed case is recorded
and later cases still run under the remaining batch budget; any failed case keeps
the batch failed. Build the ASM library test executable first with
`cargo test -p asm --lib --no-run --locked`, then pass the emitted executable:

```sh
python3 scripts/performance/prepared_source_native.py --native-test <asm-test-executable> --profile off
python3 scripts/performance/prepared_source_native.py --native-test <asm-test-executable> --profile runtime
```

Set `OPFORGE_FS_UAE_BIN`, `OPFORGE_FS_UAE_CONFIG_TEMPLATE` and
`OPFORGE_FS_UAE_ARGS` for the installed emulator as described in the
[FS-UAE guide](../../agents/rules/fs-uae.md). Reports default to unique ignored build
directories; guest protocol/output trees remain ephemeral. `--blocks 32` is an
explicit larger probe with unchanged limits, not the default inner-loop workload.

There is no native prepared-source candidate yet. The next performance claim needs
a complete mixed native path using normalized statement/symbol identities and bound
package programs, measured against this baseline. A decoder alone or Rust replay
timing cannot establish that claim. Include initial parsing/binding in total time
and prove exact output without per-pass source/package name lookup.

### Packed layout probe

The [raw block probe](../../crates/opforge-asm/src/prepared_source_experiment/packed.rs)
uses one owned byte block and borrowed readers. Its 12-byte header carries record
and symbol counts plus layout options. Each line starts with one byte storing total record length minus one, then flags
and a u16 source line, followed by only its present fields. The total includes the
prefix and trailing alignment padding and cannot exceed 256 bytes; oversized
records fail explicitly. Flags follow the length byte so the source-line word is
aligned without a redundant padding byte. Labels use u16 identities;
instruction identities independently use one or two bytes. Shared data directives
use their own statement kinds. Operands contain length-prefixed postfix expressions;
expression tags are always bytes, symbol references are u16, and constants use
explicit signed 1/2/4/8-byte payloads. Fields are big-endian. The aligned variant
pads word fields and record starts to even offsets. Overflow is rejected, not
truncated. These are probe limits, not product-wide source or symbol limits.

Only instruction-ID width varies in the token experiment; expression tags remain
fixed to avoid confusing two effects. The current NOP-only instruction IDs are S1
operation slots, pending a complete package-normalized instruction dictionary.

| Layout | 8 blocks / 488 text bytes | 32 blocks / 2,062 text bytes |
|---|---:|---:|
| Byte packed, byte instruction IDs | 492 B | 1,932 B |
| Byte packed, word instruction IDs | 508 B | 1,996 B |
| Word aligned, byte instruction IDs | 540 B | 2,124 B |
| Word aligned, word instruction IDs | 572 B | 2,252 B |

Both assembly targets have identical packed source payloads. Output endian rules
and instruction program bytes are external. Counts include header, lengths, source
locations and padding, but exclude the instruction dispatch map, encoding directory,
output, workspace and allocator overhead. The probe converts an existing typed S1
object and keeps it during validation: **this is not a measured pipeline RAM saving
or a direct packed-tokenizer implementation**. There is no compressed-string or
comment pool. This comment-free, short-line fixture offers no comment-removal saving.

With the one-byte prefix the 32-block byte-packed payload is now smaller than the
comment-free text, before adding the excluded dispatch/program storage. Alignment
and token width remain independent tradeoffs. In this revised field order word
instruction IDs add two bytes per instruction in the word-aligned profile; the
previous equality between byte/word-ID sizes no longer applies. Native read costs,
not the Rust decoder microbenchmark, must guide the eventual execution layout.

The decoder validates one pass against every original line/operand/expression field,
then times 256 bounds-checked checksum scans without comparing to the typed source.
Checksums agree across all four layouts. Tests also cover every truncation of a
complete block, integer-width boundaries, token overflow and bounded iteration
requests. Decoder timings include checksum work; they do not execute expressions,
lay out symbols or emit instructions and must not be compared with complete assembly
as a speed ratio.

```sh
cargo test -p asm --lib prepared_source_packed_benchmark --locked -- --ignored --nocapture --test-threads=1
```

For 32 blocks on the m6502 source, revised byte-packed byte/word-ID decoding took
8.13/7.90 us per scan; word-aligned byte/word-ID decoding took 8.42/8.74 us.
Packing took 5.42–6.50 us across those layouts. These are arm64 observations,
not native cycle estimates; checksum and bounds checks are included. Small host
differences do not select a native winner. Payload checksums are unchanged.

### Current validation and checkpoint limits

Ten focused tests/benchmarks pass in the uninstrumented build (8.08 seconds);
eight focused checks pass with telemetry enabled, with both timing benchmarks
ignored (0.88 seconds). The zero-lookup replay assertions still pass. Production
ASM Clippy with warnings denied, Rust formatting and the workflow gate (136 tests)
pass. The new native runner passes Python compilation, help and workload checks;
four actual native cases (two targets × telemetry on/off) passed the existing
fresh-proof runner. No native candidate or peak-RAM measurement is claimed.

The full Rust quality gate was attempted but stops at four pre-existing autofixable
redundant-test findings in `tkpkg_compact_table.asm` at lines 132, 210, 217 and 224.
That native file is unchanged by this checkpoint. The wrapper does not reach its
Clippy/audit/full-test steps; the separate affected-library Clippy result above
must not be reported as full qualification. Earlier unrelated ASM test-target
Clippy warnings also remain outside this slice. This is an experimental measurement
checkpoint, not a release-qualified integration.

Mixed-workload update validation: both targets passed with telemetry off and on
through the updated wrapper. Python compilation/help, unchanged smoke-source hash,
default workload dimensions and a mocked first-case failure followed by a successful
second case were checked. The batch correctly remains failed after a case failure.
The workflow gate passes all 136 tests. No Rust/native production source changed
in this update. The larger m68000 timeout
remains an explicitly failed measurement, not a successful larger-scale test.


### Native numeric package binding: implemented checkpoint

The native selector now carries existing CMSE numeric program-name IDs through
scalar, branch and sequence envelopes. The encoder resolves a name and performs
the existing full owner-ranked CSEM scan on a miss, then retains the validated
program pointer, length and version in a 64-entry, 12-byte-per-entry raw table.
Hits execute the same interpreter with fresh operand records, current PC and
fixups. The public named semantic-service boundary still accepts text. No new
package version or old bytecode executor was introduced.

Bindings are invalidated on package clear/load (including failed replacement)
and every successful pipeline commit. This deliberately also invalidates on a
repeated selection of the same CPU; more selective invalidation is deferred.
A full table falls back to uncached resolution. The table uses 768 bytes, plus
count/alignment and six bytes of ID/pending state across existing modules.
No output bytes or source statements are cached. Cache-capacity exhaustion,
malformed replacement packages and custom owner overrides have not received
new directed real-native coverage in this slice.

The source path remains textual. This is the first numeric package component,
not native execution of the packed-source laboratory. Its Rust reference is
package owner resolution and `selector_encoding.rs` semantic execution; its native
boundary is `selection_service` → `encode_service` with `semantic_bindings`.
The one-byte packed-line prefix is a separate Rust laboratory layout change.

Current mixed8 observations use the same inputs/package/emulator as the baseline
above. The m6502 route does not use this CSEM binding path.

| Workload | Baseline, profile off | Numeric bindings, profile off |
|---|---:|---:|
| m6502 | 4.804 s | 4.848 s |
| m68000 | 6.863 s | 6.826 s |

These are single host-observed START-to-DONE observations, not stable speed ratios.
The m68000 result is a repeat on the final code; m6502 was measured before the
sequence-boundary CCR fix on a route that does not execute that sequence path. Uninstrumented image size is 564,156 bytes
versus 563,768 baseline (+388 bytes); the package is unchanged at 368,635 bytes.
The emulator remains a 68040 configuration with additional RAM, not a physical
68020/2 MB feasibility measurement, and peak RAM is still unmeasured.

With existing reusable telemetry enabled, m68000 CSEM candidate visits fall from
23,192 to 3,791 (83.65%). Selection visits remain 360 and statement visits remain
198. m6502 encoding candidates remain zero and selection visits remain 120.
Instrumented elapsed time is approximately unchanged (m68000 6.958 → 6.859 s on the final candidate).
This establishes less repeated program resolution, not an end-to-end speedup.
Final runtime counters report no overflow. The existing counters do not separately
count binding hits or miss-time name
expansion; they establish the reduction in scanned CSEM candidates.

Correctness checks use fresh in-memory Rust oracles, explicit zero guest exit
and exact output equality (Level D). Mixed8 passes for both targets with telemetry
off and on. The added `--workload binding-switch --cpus m6502` case passes: it
alternates both CPU pipelines, varies values and uses BHS.S/BCC.S aliases, with
independently calculated output bytes in addition to the Rust oracle. Its initial
unindented fixture was rejected by Rust before guest launch and was corrected.
The existing state-guard semantic-sequence test passes after correcting the
numeric sequence decoder to test remaining bytes explicitly rather than inherit
CCR from clearing the text-length register. The existing qualified-symbol JSR
sequence/fixup case also passes on the final code.

Focused Rust checks pass (seven tests, two manual benchmarks ignored); packed
microbenchmarks were also run separately. ASM library Clippy, native formatting,
CPU/no-growth/import/inventory checks and the workflow gate (136 tests) pass.
The full quality-gate attempt still stops at the four pre-existing compact-table
redundant-test findings described above. This remains an experimental checkpoint.

Next decision: stop spending this iteration on cache tuning. Numeric bindings
remove one repeated lookup boundary, but do not explain total native time. Take
one bounded native source-record reader/replay slice next, preserving this mixed
workload for comparison, and measure where its time actually moves before
expanding the migration.

### Native binary-source execution: current experimental boundary

The native frontend reads the supplied text block line by line, runs the canonical
TKVM tokenizer once per line, and immediately writes a packed binary line. The
one-byte prefix stores total line length minus one (maximum 256 bytes). Records
contain numeric mnemonic/identifier IDs, qualifiers, literal values, punctuation
and source line numbers. Aliases bind to the same normalized mnemonic ID. This
slice uses uniform 16-bit IDs; package-dependent ID widths remain untested.

Both assembly passes consume those records. They recompute expressions, symbol
values, candidate selection, branch displacements and emitted bytes. The harness
erases the original source and every package dictionary spelling before either
pass; frontend symbol/lexeme scratch is also erased. No string reconstruction or
fallback to the textual assembler is available. The input file is still read as
one block: incremental file I/O and releasing its allocation are not implemented.

Rust prepares a provisional single-pipeline `BSP1` package capsule from the current
canonical package. It resolves immutable program names and register classes to
numeric references and copies canonical TKVM/TABL/CSEM/VALP programs unchanged.
Native execution retains the existing program interpreters. This deliberately
combines source representation and derived runtime-package preparation; timing it
against the existing CLI does **not** isolate the benefit of binary source alone.
Host capsule preparation is reported separately and excludes registry construction.
There is no old-bytecode compatibility path or adopted production package format.

The bounded syntax covers the complete mixed8 workloads: immediate values,
package-defined register operands, parenthesized member operands, forward branches,
labels, current PC, parentheses and unary/binary addition/subtraction. Shared core
handles `.cpu`, `.org`, `.byte`, `.word`, `.long` and `.end`. Unknown candidate plans
fail closed unless a necessary package match predicate proves them inapplicable.
The small numeric expression reader is provisional duplication of expression
evaluation, not a migration of the full EXVM frontend.

Restrictions: one CPU pipeline, two fixed layout passes, no general relaxation,
macros/includes/modules, scoped names, strings or general expression operators.
Expression literals must fit nonnegative signed 32-bit values; unary/binary
arithmetic checks signed overflow. Wider unsigned literals are rejected before
arithmetic, rather than reinterpreted as negative numbers.
Labels require a colon and cannot reuse reserved package spellings. `.org` cannot
create discontiguous output. Capacity limits include 64 tokenizer tokens per line,
512 source names and a 64 KiB record block. Malformed/unsupported input returns
failure with a generic harness diagnostic; full diagnostic parity is not implemented. This is a separate experimental
harness, not the normal native CLI or a completed native language replacement.

Memory is not qualified for the 2 MiB goal: the proof harness reserves about 1.5 MiB
itself and imports existing services with roughly 42 MiB of additional BSS. Erasing
text proves independence from its contents, not reduced allocation or peak RAM.
Any next migration decision must address those imported runtime responsibilities.

Reproduce the bounded comparison with the configured FS-UAE environment and a
fresh ASM test executable:

```sh
cargo test -p asm binary_source_packages_prepare
python3 scripts/performance/prepared_source_native.py \
  --native-test target/debug/deps/asm-<current-test-hash> --binary-source
```

The runner limits the batch to 150 seconds and binary guest work to 10 seconds
after START. Each success requires fresh completion, explicit zero guest exit,
equality with the current Rust oracle and independent workload bytes. Guest timing
includes native tokenization/packing, input/output and package loading, but excludes
emulator boot and host package export. Results and guest artifacts are not retained
as tracked historical evidence; record the meaningful measurements here.

Final reviewed-code observations (telemetry off, same-session serial comparison):

| Mixed8 input | Current text path | Native binary-source path | Derived capsule |
|---|---:|---:|---:|
| m6502: 688 source bytes → 104 output bytes | 4.761 s | 0.622 s | 7,268 B |
| m68000: 865 source bytes → 144 output bytes | 6.811 s | 0.606 s | 96,444 B |

Both paths match the live Rust oracle and independently calculated workload bytes
with fresh zero-exit completion (Level D). The binary path erases text before its
two passes. A preceding comparison observed 4.712/0.618 s and 7.122/0.629 s;
these short observations show a substantial combined-path improvement, not a
stable speed ratio, isolated tokenization gain or self-host prediction. The final
experimental image is 70,308 B versus 564,236 B for the current native CLI. Their
language coverage differs; this is not an equivalent full-product size comparison.
Host capsule export took 1.26 ms and 5.76 ms, excluding registry bootstrap. The
canonical text-path package remains 368,635 B. The emulator uses the existing
68040/expanded-memory configuration, not physical 68020 hardware or a 2 MiB limit.

Three VM exporter tests and the ASM capsule-preparation test pass (host evidence).
The signed-domain regression `lda #(-$ffffffff)` is rejected by the live Rust
assembler and completes natively with exit 20 and the expected diagnostic; it
cannot silently emit `A9 01`. A bounded independent review found and corrected
that arithmetic issue and two capsule-bound checks. Malformed-capsule checks were
reviewed but have not received directed guest cases. The workflow gate (136 tests),
affected-library Clippy, formatting, CPU boundary, native ownership/no-growth,
inventory and fresh-proof guards pass. The full Rust quality gate still stops at
the four unchanged compact-table redundant-test failures documented above.

This establishes a working native binary-source experiment. The agreed next steps
below preserve this gain while removing oversized legacy runtime dependencies.
The harness is not yet ready for the normal CLI or the 2 MiB product target.

## Next implementation: compact native runtime

Status: planned; approach agreed, implementation not started. Start with M1;
review its working result before moving to M2. The plan-authoring skill, active
AGENTS.md and workflow linked above remain binding.

The outcome is the same working binary-source subset with small owned state and
completed 68020 / AmigaOS 3.1+ cases within **2 MiB total installed RAM**, including
the OS. Reference checkpoint: `a378ec48`; the measured timing, image/package sizes
and provisional memory breakdown immediately above are the baseline. Confirm
the allocation breakdown from the linked image before choosing the first dependency
to remove. This is a step toward the product goal, not self-host qualification.

### Implementation checkpoints

| Item | State | Inspectable result |
|---|---|---|
| M1 — Detach interpreter execution from legacy assembler state | Next | Both mixed8 cases work through shared interpreters with the large legacy state dependency removed; linked-memory comparison identifies what disappeared |
| M2 — Own memory by lifetime and qualify the constrained runtime | Pending M1 review | Right-sized allocations, preparation storage actually released, bounded scaling results, and completed 2 MiB guest cases |

#### M1: one coherent interpreter boundary

Trace the experiment's imports to the large allocations, then separate the
required TABL/CSEM/VALP execution responsibility from the surrounding selection,
service and full-assembler state. Reuse canonical program interpreters with explicit
small contexts and bounded input/output buffers. The existing native path must use
the same maintained interpreter implementation through thin adapters where needed;
do not fork the VM implementations or copy target semantics into native code.

Rust reference: canonical package execution and selector/value semantics. Native
starting boundaries: `experimental/binary_encoding.asm`,
`tkpkg/tkpkg_encode_service.asm`, `tkpkg/tkpkg_selection_service.asm` and their
actual state dependencies, all under `native/motorola68000/amigaos/`.
Keep the current CLI runnable; remove superseded interpreter bodies when sharing
their replacement. Do not extract unrelated legacy services just to tidy files.

Done: mixed8 on both targets passes the fresh native proof contract; affected
existing interpreter/service contracts pass; the linked-image breakdown proves
which legacy allocations are no longer reachable. Measure final image size and
unprofiled mixed8 time against the reference on the same emulator configuration.
This checkpoint need not yet fit 2 MiB: its owned remaining storage is M2's work.
If removing the dependency requires a larger semantic migration, stop at a working
recovery point and discuss the specific boundary rather than expanding the rewrite.

#### M2: preparation and execution memory lifetimes

Separate immutable numeric package/source storage, mutable pass state, and temporary
lexical/binding workspace. Use compact raw blocks and named layouts; allocate from
actual requirements or explicit bounded growth rather than maximum legacy capacities.
Account for growth/copy peaks, alignment, stack, output and allocator overhead.
Read source incrementally into the line tokenizer and its packed-record writer.
Discard consumed source and release binding dictionaries/scratch before assembly;
clearing bytes while retaining their allocation does not meet this requirement.
Retain numeric source locations; original text may be reread only for diagnostics.

If the provisional capsule must change to separate lexical and executable storage,
update its Rust producer and native consumer together. Retain only the new contract
and reject mismatches; no compatibility layer or permanent disk-format commitment.
Host package preparation remains explicit and outside guest timing.

Use reusable, compile-time-gated telemetry for preparation/assembly phases and
memory accounting. Verify that disabled builds omit telemetry code, storage and
imports. Report instrumented accounting separately from release timing.

Done: both targets complete mixed8 and mixed32 with exact output, no text lookup
after preparation, and no material unexplained timing regression. Demonstrate the
same cases on a reproducible 68020 / AmigaOS 3.1+ guest with 2 MiB installed RAM.
Record effective CPU/memory settings, OS image/version, free memory and largest
free block before launch, executable/BSS/stack requirements, and peak runtime
allocations including transient overlap. Account for OS/resident memory and report
remaining headroom; a 2 MiB application allocation budget on a larger guest is not
the acceptance test. Check that the runner does not silently restore its current
64 MiB Zorro III override or other expansion memory. Constrained timing is a
separate result from the existing 68040 comparison, not a calibrated hardware claim.

### Validation, limits and follow-on decision

- Keep existing mixed8/mixed32 source generation, varied operands, forward labels,
  shared data emission and complete-case meaning. Never reduce inputs to fit the
  implementation. Current arithmetic rejection remains a regression contract.
- Use focused host checks for capsule/layout/lifecycle changes and real-native
  positive/negative contracts for the affected boundary. Preserve fresh challenges,
  completion, explicit guest exit, live Rust oracles and ephemeral guest artifacts
  under the [parity contract](../../agents/rules/native-rust-parity-porting.md).
- Reuse the bounded measurement scripts: at most 60 seconds per invocation,
  150 seconds per batch, and 10 seconds after START for the binary path. Run
  comparison and constrained-memory batches separately. Do not rerun the old
  non-completing large case or self-host, automatically increase deadlines, or
  turn a timeout into a timing result. Mixed32 measures candidate scaling; an
  unavailable old-path result is explicitly missing, not required to finish M2.
- Report source/record/package/image bytes, linked reserved memory, peak allocated
  memory, released preparation storage, phase work and unprofiled end-to-end time.
  Separate fixed startup from growth with input size. Investigate a substantial
  regression with one focused discriminator rather than prolonged sampling.
- Each checkpoint ends with a working result, focused evidence and a local recovery
  commit. Keep details and the two status rows here current; Git holds historical
  artifacts. Existing unrelated gate failures remain explicit qualification limits.

Do not add language features, variable-width IDs, target-specific fast paths or
attempt general CLI integration in M1/M2. After review, choose the next small
coverage slice from representative real source—expressions, macros or modules—
with explicit scope and tests. The numeric-source boundary remains mandatory.
Do not automatically start that next slice or retain parallel experimental products
after a replacement has been qualified for integration.
