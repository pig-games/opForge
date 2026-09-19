# String-free assembly replay experiment

Status: M6 measured: tokenization and name binding dominate preparation; compact
expression compilation is a smaller share. Baseline is M5 `e46f1a6f`; release
image, coverage, representation and the normal native CLI remain unchanged.
M1/M2 completed mixed8 and mixed32 on both targets in a 68020 / 2 MiB guest;
see the [compact runtime result](#m2-result-2026-09-19). The normal native CLI remains the reference.
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

The native frontend receives incrementally read source lines, runs the canonical
TKVM tokenizer once per line, and immediately writes a packed binary line. The
one-byte prefix stores total line length minus one (maximum 256 bytes). Records
contain numeric mnemonic/identifier IDs, qualifiers, literal values, punctuation
and source line numbers. Aliases bind to the same normalized mnemonic ID. This
slice uses uniform 16-bit IDs; package-dependent ID widths remain untested.

Both assembly passes consume those records. They recompute expressions, symbol
values, candidate selection, branch displacements and emitted bytes. Before either
pass, the harness frees line/input buffers, frontend symbol/lexeme scratch and the
full lexical package. Only a relocated executable prefix and numeric records remain.
No string reconstruction or fallback to the textual assembler is available.

Rust prepares a provisional single-pipeline `BSP2` package capsule from the current
canonical package. It resolves immutable program names and register classes to
numeric references and copies canonical TKVM/TABL/CSEM/VALP programs unchanged.
Native execution retains the existing program interpreters. This deliberately
combines source representation and derived runtime-package preparation; timing it
against the existing CLI does **not** isolate the benefit of binary source alone.
Host capsule preparation is reported separately and excludes registry construction.
There is no old-bytecode compatibility path or adopted production package format.

The bounded syntax covers the complete mixed8 and mixed32 workloads: immediate values,
package-defined register operands, parenthesized member operands, forward branches,
labels, current PC, parentheses and unary/binary addition/subtraction. Shared core
handles `.cpu`, `.org`, `.byte`, `.word`, `.long` and `.end`. Unknown candidate plans
fail closed unless a necessary package match predicate proves them inapplicable.
At the M2 baseline, the small numeric expression reader duplicated evaluation.
M3 below replaces it with compilation and shared ExprVM execution; it is still
not a migration of the full EXVM frontend.

Restrictions: one CPU pipeline, two fixed layout passes, no general relaxation,
macros/includes/modules, scoped names, strings or general expression operators.
Expression literals must fit nonnegative signed 32-bit values; unary/binary
arithmetic checks signed overflow. Wider unsigned literals are rejected before
arithmetic, rather than reinterpreted as negative numbers.
Labels require a colon and cannot reuse reserved package spellings. `.org` cannot
create discontiguous output. Capacity limits include 64 tokenizer tokens per line,
512 source names, 4 KiB textual lines, 256-byte packed lines, and a 1 MiB ceiling
per growing block. These are explicit experimental limits, not preallocations. Malformed/unsupported input returns
failure with a generic harness diagnostic; full diagnostic parity is not implemented. This is a separate experimental
harness, not the normal native CLI or a completed native language replacement.

M1 removed the imported legacy state. M2 qualifies the listed workloads on a
2 MiB guest, with about 20 KiB linked reservation and measured owned-allocation
peaks below 271,000 bytes. This is subset qualification, not full-language or
self-host qualification; see the current results below.

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

Reference `a378ec48` observations (telemetry off, same-session serial comparison;
BSP1 and the former fixed-allocation harness):

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

This established the binary-source reference for M1/M2 below. General CLI
integration remains outside the qualified subset.

## Next implementation: compact native runtime

Status: M1/M2 implemented; focused validation and remaining limits are recorded
below. This result is the baseline for the authorized M3 experiment. The plan-authoring skill, active
AGENTS.md and workflow linked above remain binding.

The outcome is the same working binary-source subset with small owned state and
completed 68020 / AmigaOS 3.1+ cases within **2 MiB total installed RAM**, including
the OS. Reference checkpoint: `a378ec48`; the measured timing, image/package sizes
and provisional memory breakdown immediately above are the baseline. Confirm
the allocation breakdown from the linked image before choosing the first dependency
to remove. This is a step toward the product goal, not self-host qualification.

### Binary representation invariant

Binary source and package representations contain **no actual memory pointers**.
References use numeric IDs or offsets from a defined base: the whole block or a
specified enclosing region. Each offset field has an unambiguous base, width and
bounded target; nested offsets are allowed. Validate ranges and arithmetic overflow
before resolving references. Moving a block to a different address must not require
patching its records. Test that property by copying the representation to another
base and obtaining the same result. Live execution contexts may resolve pointers
for access, but those pointers remain outside the binary representation.

This preserves the option of an editable tokenized source file on constrained
platforms. Optional formatting metadata and persistent package-vocabulary identity
remain a later slice; M1/M2 do not introduce that file-format contract.

### Implementation checkpoints

| Item | State | Inspectable result |
|---|---|---|
| M1 — Detach interpreter execution from legacy assembler state | Complete; reviewed | Both mixed8 cases work through shared interpreters with the large legacy state dependency removed; linked-memory comparison identifies what disappeared |
| M2 — Own memory by lifetime and qualify the constrained runtime | Implemented; review before further coverage | Right-sized allocations, preparation storage actually released, bounded scaling results, and completed 2 MiB guest cases |

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
This checkpoint did not need to fit 2 MiB; its remaining owned storage was M2's work.
If removing the dependency requires a larger semantic migration, stop at a working
recovery point and discuss the specific boundary rather than expanding the rewrite.

#### M1 result (2026-09-19)

TABL/CSEM execution now lives in `tkpkg.amigaos.encoding_execution`, with a
56-byte caller-owned runtime context and bounded output/fixup buffers. VALP lives
in the stateless `tkpkg.amigaos.value_execution`. The existing native text services
adapt their state to those same interpreters; their former interpreter bodies and
experimental numeric wrappers are removed. The binary path owns a 4 KiB output
buffer and no longer imports selection services, legacy package buffers or assembler
state. No canonical program, source-record or BSP1 representation changed; resolved
pointers exist only in runtime contexts, outside the offset-only binary blocks.

Fresh captured HUNK_HEADER reservations, with profiling disabled:

| Linked resource | Reference `a378ec48` | M1 |
|---|---:|---:|
| Executable file | 70,308 B | 16,048 B |
| Code reservation | 60,464 B | 14,456 B |
| Data reservation | 3,452 B | 316 B |
| BSS reservation | 44,290,044 B | 1,534,512 B |
| Total linked reservation | 44,353,960 B | 1,549,284 B |

The import removal eliminates a net 42,755,532 B of BSS, including the legacy
package/selection/assembler allocations. Remaining storage is owned by the binary
harness, frontend and execution helpers. These are linked reservations, **not peak
RAM**: heap, stack, loader and OS costs are not included. M2 still needs to right-size
buffers and release preparation storage; this does not qualify a 2 MiB guest.

One fresh observation per path/CPU on the same A4000/68040 configuration, 2 MiB
chip + 8 MiB fast RAM and the runner's 64 MiB Zorro III override:

| mixed8 target | Reference binary path | M1 binary path | Current text path |
|---|---:|---:|---:|
| m6502 (688 B source, 104 B output) | 0.5840 s | 0.0500 s | 4.6167 s |
| m68000 (865 B source, 144 B output) | 0.6145 s | 0.0300 s | 6.7909 s |

All four current cases and both reference cases completed with fresh guest zero
exit and exact live Rust output; the current batch additionally checked independent
workload bytes. The binary passes retain the source/dictionary erasure proof.
START-to-DONE includes executable loading/initialization and I/O, excludes emulator
boot and host package preparation. At 20 ms host polling, the new times are too
short for a precise speedup ratio or isolated interpreter-cost claim. Reduced load
and initialization work is a plausible contributor, not separately measured here.
Runtime capsules remain 7,268 B / 96,444 B; the text executable is 564,624 B.
Captured binary image digests (FNV-1a64) are `7b5fa2cd78e871bc` for the reference
and `a857005f7ac4cdfb` for M1.

Reproduce with `scripts/performance/prepared_source_native.py` and arguments
`--native-test <current-asm-test-binary> --binary-source`, using the FS-UAE environment
above. For the old
binary path, materialize `a378ec48`'s `native` tree and `.opforgefmt.toml` in a temporary
root, then run `tests::binary_source_experiment::binary_source_fs_uae` with the same
source/CPU, `--exact --ignored`, and test-only `OPFORGE_COMPARE_NATIVE_ROOT` pointing
to that root. Producer and native capsule versions must match: reconstruct historical
BSP1 runs using the corresponding Rust checkpoint, not the current BSP2 producer.
The live Rust oracle and fresh-run safeguards still apply. The Hunk
accounting helper rejects malformed/truncated load files instead of estimating BSS
from file size. Test artifacts remain ephemeral; this note retains the comparison.

Focused validation: both mixed8 text/binary pairs, state-guard sequence continuation,
qualified JSR, required value-program expression, absolute-long fixup, and binary
arithmetic rejection passed real-native checks. The TRAP #16 diagnostic contract
**fails identically on M1 and unmodified `a378ec48`**: both produce `OTR901: encode
table malformed` instead of the expected range diagnostic. This is an existing
native parity gap, left explicit rather than weakening the expected diagnostic.
M1 is qualified for the listed passing cases, not full native parity.

Host validation passed: package preparation and three Hunk-accounting tests;
telemetry enabled/disabled byte-transparency test; affected-library Clippy; Rust
format; native instrumentation/contracts/invocation/no-growth/proof/format gate;
updated ownership inventory; CPU boundary checks; 136 workflow tests. The affected
five assembly files have no mechanically redundant-test findings (70 advisory
call/CCR sites remain). No full self-host or long measurement suite was run.

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

#### M2 result (2026-09-19)

The frontend now has begin/line/finish operations over caller-owned preparation
scratch. Buffered reads feed one textual line directly to the tokenizer and packed
writer; no whole-source allocation remains. Record storage grows geometrically
from 256 bytes, with a 1 MiB per-block ceiling and old/new overlap accounted for.
Symbol arrays are sized from the prepared numeric ID extent. The first assembly
pass sizes output; a caller callback allocates it before pass two. All failure and
success paths release owned blocks. Generic directives and canonical VM programs
are unchanged; the normal CLI still uses M1's shared interpreter implementation.

Latest-only BSP2 has a 76-byte header, retaining prior field offsets and adding
`RuntimeBytes` at offset 72. Rows, projections and programs occupy the executable
prefix; dictionary and tokenizer data occupy the tail. After preparation, native
code copies the prefix to a different allocation, clears its lexical metadata and
frees the full capsule and preparation workspace. Binary references remain offsets;
producer bounds/relocation tests and real-native relocated execution pass. BSP1
is rejected. There is no new persistent source-file format or compatibility layer.

Release image: **17,012 B** (`fnv1a64:b34280f43ed3fbd8`); linked reservation:
**20,148 B** (15,344 code, 316 data, 4,488 BSS), versus M1's 1,549,284 B.
The separately instrumented image reserves 20,692 B. Memory telemetry has no code,
data or imports in release; host byte-transparency tests cover either absent gate.

| Target / blocks | Source / packed records | Capsule / retained prefix | Peak owned allocation | Live allocation after assembly | Release time, 68020 / 2 MiB |
|---|---:|---:|---:|---:|---:|
| m6502 / 8 | 688 / 900 B | 7,272 / 6,546 B | 42,496 B | 25,856 B | 0.470 s |
| m6502 / 32 | 2,788 / 3,492 B | same | 47,104 B | 29,184 B | 1.148 s |
| m68000 / 8 | 865 / 1,076 B | 96,448 / 91,298 B | 264,192 B | 166,144 B | 1.187 s |
| m68000 / 32 | 3,493 / 4,196 B | same | 270,336 B | 173,056 B | 3.624 s |

The packed records are currently larger than these comment-light sources; no
compression gain is claimed. Preparation workspace is bounded at 22,860 B plus
4 KiB I/O, 4 KiB line and 256 B packed-line buffers (one 32 KiB allocation).
Power-of-two growth leaves capacity slack. The largest peak is dominated by the
old and relocated 128 KiB package allocations overlapping, not assembly output.
Cumulative capacities freed before assembly are 41,728 / 44,800 B (6502) and
165,632 / 171,776 B (68000), including replaced record blocks. Every instrumented
case reports zero live allocations after cleanup and balanced allocation/free totals.

The reproducible guest retains the installed A4000 template and Kickstart image,
sets CPU=68020, chip RAM=2048 KiB, and disables fast/slow/motherboard/Zorro/RTG
memory. Guest commands confirm `68020 68882`, Kickstart 47.96, Workbench 47.2,
a 4,096-byte stack, 2,080,768 managed chip bytes and **zero fast RAM**. No FPU
instructions are used by this implementation. Before program launch, available
memory was 1,226,728 B and the largest block approximately 1,221,496–1,222,712 B.
After executable loading, telemetry observed about 1,205,712 B free. The largest
tracked peak plus instrumented linked reservation and configured stack totals
295,124 B; roughly 0.93 MB remains relative to entry free memory after subtracting
owned peak allocation. This is headroom accounting, not an OS-wide peak profiler:
loader/OS allocations are reflected in available-memory observations, while telemetry
counts this implementation's requested capacities and their transient overlap.
Actual completion on the constrained guest is the memory acceptance proof.

Timings are one START-to-DONE observation per case, with 20 ms polling; they include
loading and I/O and exclude emulator boot and host package preparation. Mixed32
was remeasured on the final release binary; mixed8 preceded a redundant-test removal
and clearing two unused preparation-pointer fields. On the original expanded-memory
68040 comparison configuration, mixed8 measured 24 ms for 68000; the 6502 case
completed too quickly for a separate START timestamp. There is no measurable large
regression against M1's 30–50 ms observations, but neither a precise speedup ratio
nor calibrated physical-hardware timing is established. Do not compare the 68020
column directly with those 68040 times or extrapolate it to self-host completion.

Reproduce using the configured FS-UAE environment and a current test executable:

```sh
python3 scripts/performance/prepared_source_native.py \
  --native-test target/debug/deps/asm-<current-test-hash> \
  --binary-source --binary-only --memory-profile 2m --blocks 32
```

Run blocks 8 separately; add `--compare-memory` for the separate accounting build.
Use `--memory-profile existing` for the earlier 68040 configuration. Reports record
the effective config, input/image identities, completion and memory receipts.
Each batch remains capped at 150 seconds, each invocation at 60 seconds, and binary
work at 10 seconds after START. Saved user configuration and disks are not edited.

Both targets pass mixed8/mixed32 exact live Rust and independent workload-byte
comparisons in release and accounting builds. A 6,753-byte CRLF variant with a
long comment, a 4 KiB read-boundary crossing and no final newline also passes on
the final code. The signed-arithmetic rejection completes with exit 20 and proves
all failure-path allocations released. Host package/relocation, telemetry gating,
runner configuration/capture, Clippy, formatting, native guards and workflow checks
pass. General language coverage, physical-machine timings, memory-exhaustion fault
injection, and full diagnostic parity remain unqualified. The pre-existing TRAP
diagnostic mismatch recorded under M1 remains outside this change.

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


## M3 — Prepare expressions once

Approved 2026-09-19; implemented and measured as an experimental checkpoint.
Baseline: `a8f707f1` (M2).

**Hypothesis:** preserving expression structure as compact executable numeric
operations removes repeated precedence/parenthesis parsing from assembly passes
and candidate evaluation. Preparation and record growth may outweigh that saving
for short inputs; measure the complete tradeoff before claiming improvement.

**Inspectable outcome:** the streaming frontend prepares expressions line by line;
assembly only evaluates numeric operations against current symbol values and PC.
Original text and lexical dictionaries are released before execution as in M2.
No pointers enter the representation. Expressions in shared data directives and
instruction operands use the same evaluator. Package register/member syntax and
instruction selection remain package-owned; this is not early assembly.

Scope starts with the existing unary/binary plus/minus, grouping, numeric symbols
and current-PC forms. Add multiplication with normal precedence, motivated by
actual native layout expressions such as `TOKENS+64*20` and
`ENTRIES+SYMBOL_LIMIT*8` in `binary_frontend.asm`. Use reduced, complete sources
that exercise those shapes without requiring equates, modules or macros. Do not
claim those larger source files can already assemble through this path.
Investigate sharing existing ExprVM arithmetic before extending the provisional
reader. Preserve the explicit signed-32-bit subset and overflow rejection in this
slice; full i64 and additional operators remain outside its qualification.

Implementation and evidence:

- Introduce bounded compiled expressions in packed records, with numeric symbol
  IDs and explicit lengths. Compile once; no parsing/text fallback during passes.
  Keep the old implementation recoverable at the baseline commit, not as a new
  permanent production switch or version-compatibility branch.
- Validate precedence, nesting, unary operators, current PC, forward symbols,
  malformed/truncated expressions, overflow and bounded stack/record rejection.
  Compare completed native output with the live Rust oracle and independent bytes.
- Retain mixed8/mixed32 on both targets. Add `expression-replay` (existing syntax)
  for identical-input before/after comparison and `expression-layout` (multiplication)
  for new coverage. Unsupported baseline cases are not timing comparisons.
- Use the same 68020 / 2 MiB guest. Report release end-to-end time, prepared bytes,
  peak allocation and linked image size; distinguish preparation and evaluation
  work through gated reusable telemetry where available. Instrumented timings
  never substitute for release timing. Preserve the 10-second post-START,
  60-second invocation and 150-second batch bounds; no full self-host run.

Success: correct completed cases, no expression-structure parsing after preparation,
useful additional coverage, and explained measured time/memory costs. Stop to
reconsider if reuse requires importing string lookup/legacy state, package syntax
must be guessed, or regressions erase the benefit. Do not widen the step to macros,
modules, relaxation, general CLI integration or persistent binary file formats.

End with a focused local commit, updated results and a comparison against M2.
No remote push is included.


### M3 implementation and measurements

The native frontend now compiles each scalar expression once, immediately after
numeric token lowering for that line. Packed scalar fields contain
`0x80, u8 program_bytes, ExprVM-v2 bytes`; literals and symbol operands inside
that existing VM contract are little-endian, while the enclosing source record
retains its big-endian numeric fields. Programs use global numeric symbol IDs,
contain no pointers, and fit inside the existing 256-byte line bound. Register
names and parenthesized member wrappers stay outside scalar programs. Operand
shape readers skip the complete compiled block, never its interior literal bytes.

The old recursive expression evaluator is replaced by a small adapter to the
shared native ExprVM. Its new checked-signed-32 entry enforces the existing subset's
intermediate overflow policy on every stack push, requires complete program
consumption, and restores the caller's opcode selection. The ordinary i64 entry
retains its domain. No name table is used by native execution; its misleading
old parameter comment was corrected. The compiler supports multiplication plus
existing additive/unary/grouped forms. Eight value-stack slots and sixteen syntax
nesting levels are explicit bounds. Full i64 literals, other operators, macros,
modules and general CLI integration remain outside this experiment.

Identical-input release comparison against M2 `a8f707f1`, same 68020 / 2 MiB
configuration and live Rust output checks:

| Expression-replay workload | M2 | M3 | Observed change |
|---|---:|---:|---:|
| m6502 / 8 | 0.656 s | 0.712 s | +8.5% |
| m68000 / 8 | 1.435 s | 1.505 s | +4.9% |
| m6502 / 32 | 1.977 s | 2.183 s | +10.4% |
| m68000 / 32 | 4.484 s | 4.693 s | +4.7% |

These are single START-to-DONE observations with 20 ms polling, not statistically
qualified ratios or physical-hardware calibration. They include preparation and
I/O, exclude host package preparation and emulator boot, and do **not** establish
a performance improvement. The eight-block 6502 candidate preceded the member-reader correction needed for
68000; final 32-block release results use the completed implementation.

Separate instrumented multiplication workload (`expression-layout`, 32 blocks):

| Target | Source / packed bytes | Peak owned allocation | Compiled / evaluated | Preparation / assembly |
|---|---:|---:|---:|---:|
| m6502 | 6,741 / 8,107 | 53,248 B | 225 / 514 | 1.62 / 0.44 s |
| m68000 | 7,478 / 8,811 | 278,528 B | 225 / 578 | 2.58 / 2.00 s |

Both targets emit 4,842 expression-program bytes. Counts include `.org` and
candidate evaluations; they prove compilation/evaluation work, not that every
repeated evaluation can be eliminated. DOS DateStamp phase measurements have
20 ms resolution and include accounting overhead. They are not release timings,
and no before/after phase-speedup claim is made. Every accounting run releases
all owned allocations, balances allocation/free totals and completes within 2 MiB.

The current reusable accounting record is MEM3 (112 bytes), adding compile/evaluate
counts, program bytes and three phase clocks. Both debug/accounting gates remain
required; disabled builds omit code, data, imports and accounting I/O. The older
record is superseded. The shared allocator's capacity slack and relocation overlap
remain visible; packed records are larger than these comment-light sources.

**Decision:** preserve this as a measured experimental checkpoint, not a speedup
or a migration into the main CLI. The structural objective and multiplication
coverage work, but the total-time hypothesis is not supported by these observations.
Before widening language coverage, discuss a bounded revision focused on preparation
cost and expression representation. Reusing eight-byte literal payloads is a concrete
size cost; whether a compact runtime encoding or constant folding pays for itself
still requires a separate measured experiment.

Reproduction: use the M2 native tree as `--native-source-root <baseline-root>` and
current native tree as the default, with the same current test binary:

```sh
python3 scripts/performance/prepared_source_native.py \
  --native-test target/debug/deps/asm-<current-test-hash> \
  --binary-source --binary-only --memory-profile 2m \
  --workload expression-replay --blocks 32
```

Use `--workload expression-layout` for multiplication and add `--compare-memory`
only for separate accounting runs. `mixed` remains the default regression workload.
The baseline cannot execute multiplication; it is never counted as a timing result.
Release image: **19,828 B**, `fnv1a64:a0860a07268a1333`; linked reservation:
**22,912 B** (18,028 code, 316 data, 4,568 BSS). M2 reserved 20,148 B.
The separately instrumented image reserves 23,772 B. Both images remain small
relative to the measured 2 MiB guest budget.

Final release regressions complete with exact live Rust and independent bytes:

| Workload | m6502 | m68000 |
|---|---:|---:|
| mixed8 | 0.490 s | 1.300 s |
| mixed32 | 1.244 s | 3.714 s |
| expression-layout32 | 2.174 s | 4.696 s |

The first 68000 layout-release invocation hit the unchanged 60-second host limit
without a result receipt. A bounded retry completed; the timeout supplies no timing
or native failure claim. Source/program semantics were not changed for that retry.

Focused qualification: mixed8/mixed32 and expression-replay8/32 complete on both
targets; expression-layout32 completes in release and accounting builds. The
positive boundary case covers exactly eight stack values, sixteen grouping levels,
unary plus, multiplication precedence and both signed32 endpoints. Rejection
contracts cover intermediate overflow, compiled-record capacity, excess value-stack
and syntax depth, incomplete expressions and high-bit literals. The original M2
`lda #(-$ffffffff)` rejection also passes, with fresh nonzero
exit/diagnostics and balanced cleanup. Rust acceptance is checked separately:
`.byte -$ffffffff` is accepted by Rust data emission but deliberately outside this
native subset, not a syntax/parity failure.

The shared evaluator's existing **414 real-native i64 cases** pass. Host package,
Hunk-allocation/relocation bounds, telemetry gating, workload byte-contract tests,
formatting, library Clippy, workflow boundaries and staged native engineering
checks pass. Broad `cargo clippy -p asm --tests -- -D warnings` still fails on five
findings in unchanged test infrastructure (`large_enum_variant`,
`assertions_on_constants`, two `manual_range_patterns`, and `useless_vec`); these
are not included in this change. No full self-host, physical-hardware performance,
general language parity or arbitrary persisted-bytecode fuzzing is claimed.

The focused native boundary/rejection tests are named
`binary_expression_boundary_fs_uae` and `binary_expression_limit_*_fs_uae` in
`crates/opforge-asm/src/tests/binary_source_experiment.rs`. Run each ignored test
individually with the configured FS-UAE environment, `OPFORGE_COMPARE_MEMORY=1`,
`OPFORGE_FS_UAE_MEMORY_PROFILE=2m`, `OPFORGE_FS_UAE_TIMEOUT_MS=60000` and
`OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=10000`. These preserve the same per-case
bounds and fresh-run/cleanup proof contract as the comparison runner.

## M4: fold constants during preparation

Agreed scope: reduce repeated expression work without changing the runtime bytecode
format or widening language coverage. Compile and validate as in M3, then replace
constant-only subexpressions with literals: `5*3-2` becomes `13`, while
`label+(3*2)` retains the symbol and becomes `label+6`. Never fold symbols or the
current address, reassociate operators, or hide an intermediate signed32 overflow.
Reuse the checked shared evaluator rather than introduce a second arithmetic engine.
Original syntax, pre-fold program-size and stack limits remain unchanged. Temporary
compiler scratch may contain pointers; stored programs continue to contain only
values and numeric identities.

Baseline: M3 `68cd4c73`, snapshotted outside the working tree. Compare identical
unchanged expression-replay32 and expression-layout32 inputs for both m6502 and
m68000 in the same 68020 / 2 MiB guest. Include preparation in release totals;
separate instrumented runs report program bytes, compile/evaluation counts, phase
times and allocation cleanup. Preserve 10-second post-start, 60-second invocation
and 150-second batch limits. No full self-host or broad benchmark expansion.

Correctness: exact live Rust and independent workload bytes, plus constant and
mixed dynamic subtrees, precedence, signed endpoints, intermediate overflow and
unchanged capacity rejection cases. Inspect linked image and scratch growth.
Success requires a reproducible total-time benefit, not merely fewer bytes or
operations. If costs outweigh savings or results are inconclusive, retain an honest
experimental checkpoint and reassess; do not migrate it into the production CLI.
Compact literal/runtime encoding and further coverage are separate future decisions.

### M4 result

The compiler now folds maximal constant-only subtrees after original validation.
A bounded postfix scan records their spans; a compacting copy evaluates each
nontrivial span once with the shared checked evaluator and writes a standard
eight-byte literal payload. Programs of ten bytes or less bypass the folder.
There is no second arithmetic implementation, new bytecode version or persistent
pointer. Temporary scratch is 288 bytes plus saved registers, inside the existing
4096-byte guest stack; allocation telemetry does not count this stack scratch.

Identical-input release measurements on the same 68020 / 2 MiB configuration:

| Workload / target | M3 | M4 | Observed change |
|---|---:|---:|---:|
| expression-replay32 / m6502 | 2.152 s | 2.246 s | +4.4% |
| expression-replay32 / m68000 | 4.664 s | 4.780 s | +2.5% |
| expression-layout32 / m6502 | 2.159 s | 2.232 s | +3.4% |
| expression-layout32 / m68000 | 4.712 s | 4.757 s | +0.9% |

These are single observations with 20 ms polling, not statistically stable ratios.
They include preparation and I/O; every case has exact live Rust and independent
workload output checks. They establish no total-time improvement. The unchanged
benchmark definitions and bounded commands from M3 apply, using a native snapshot
of `68cd4c73` for `--native-source-root` instead of M2.

Release image: 20,224 bytes, `fnv1a64:75150875487e3fd5`; linked reservation:
23,300 bytes (388 bytes more than M3). No new persistent storage or telemetry
format is introduced. Existing compiled-program bytes report the folded payload;
the evaluation counter still counts assembly calls, not preparation-time folding.

The focused folding fixture checks exact output and a 126-byte total across ten
compiled expressions, including constants on both sides of forward references,
current-address expressions, negative literals and signed endpoints. Its first
draft used reserved label `end`; both M3 and M4 rejected it. The corrected fixture
uses `fold_target`, retaining the existing package-name reservation boundary.

Separate instrumented expression-layout32 comparison (M3 → M4):

| Target | Packed records | Peak owned allocation | Preparation | Assembly |
|---|---:|---:|---:|---:|
| m6502 | 8,107 → 6,571 B | 53,248 → 53,248 B | 1.60 → 1.64 s | 0.44 → 0.38 s |
| m68000 | 8,811 → 7,275 B | 278,528 → 270,336 B | 2.58 → 2.58 s | 2.00 → 1.96 s |

Expression payloads shrink from 4,842 to 3,306 bytes on both targets (31.7%).
Counts remain 225 compiled and 514/578 assembly evaluations. Both runs clean up
all owned allocations. Records now occupy slightly less space than these source
files, but runtime/package tables and allocation capacity remain separate costs.
The 68000 retained allocation falls by 8 KiB; 6502 capacity stays unchanged despite
the smaller payload. Phase clocks have 20 ms resolution and accounting overhead;
their small gains do not override the separate release-time observations.
One 6502 accounting invocation hit the unchanged 60-second host deadline without
a receipt; a bounded retry completed. The timeout supplies no timing or native
failure result.

**Decision:** stop at this measured checkpoint. Folding demonstrably reduces
stored work and helps assembly-phase cost, but this post-compilation scan/copy
does not establish an end-to-end speedup. Before another implementation step,
discuss folding while compiling versus a more compact runtime literal encoding;
neither change is automatically activated by this result. No production CLI
migration or broader language coverage is included.

Qualification: the native folding fixture and positive boundary fixture pass;
additive, multiplicative and unary-negation intermediate overflow are rejected;
the original program-size, stack and nesting rejection cases pass. These eight
focused cases include fresh completion and balanced allocation cleanup. Package
preparation, unchanged workload byte-contract tests, Rust formatting/library Clippy,
workflow boundaries, the staged native engineering gate and explicit formatting
of all 21 experimental harness modules pass. The shared evaluator was unchanged;
its broader M3 qualification is not claimed as a fresh M4 run. No general language
parity, physical-hardware timing or full self-host qualification is claimed.

## M5: compact runtime expressions

M4's size reduction is valuable even without a total-time win; retain folding.
Hypothesis: typed signed 1/2/4-byte literals and single-byte arithmetic operators
reduce retained expression bytes and decoding work without duplicating arithmetic.
Fold and compact in the existing preparation traversal. The shared native VM reads
the compact form directly; do not expand it to canonical bytes on every evaluation.

The compiler still validates the same bounded canonical expression and the folder
uses its checked arithmetic. Only the retained runtime form changes. Use a new
prepared-expression tag, replace the previous reader contract, and reject old or
malformed forms. This is an experimental runtime representation, not another
canonical package version or a legacy compatibility executor. Preserve symbolic
IDs, PC dependence, evaluation order, signed32 intermediate overflow and existing
pre-fold program/stack/syntax limits. No target-family logic or stored pointers.

Compare unchanged expression-replay32 and expression-layout32 for m6502/m68000,
same 68020 / 2 MiB configuration, against a snapshot of M4. Report release totals,
separate accounting phase times, program/record sizes, linked image and allocation
cost. Keep 10-second post-start, 60-second invocation and 150-second batch limits.
Exact live Rust and independent workload bytes remain required. Add a batched
direct evaluator contract for width boundaries, checked arithmetic and malformed
programs; qualify the shared general evaluator as well as source-level folding.

Success can be worthwhile size savings with acceptable timing, not solely a speed
ratio. Stop and review a material time/code-size regression or architectural
complexity. No new operators, wider arithmetic domain, CLI migration or additional
optimization step is activated by this experiment.

Provisional runtime layout: `0x81, u8 payload_bytes, payload`. All multibyte
payload fields are little-endian; the outer source record retains its existing
layout. The previous `0x80` wrapper is superseded, not accepted as another mode.

| Payload opcode (hex) | Following bytes | Meaning |
|---|---|---|
| `00` | none | End; exactly one value and no trailing bytes |
| `11` | none | Current address |
| `12` | u16 | Numeric symbol ID |
| `13` / `14` / `15` | i8 / i16 / i32 | Sign-extended literal, narrowest fitting width |
| `30` | none | Negate |
| `31` / `32` / `33` | none | Add / subtract / multiply |

For example, a folded constant now occupies 3, 4 or 6 payload bytes including
END, rather than 10. `label+6` occupies 7 rather than 15. Width selection depends
on the value, not the assembly target. The native evaluator selects its decoder
once per call using a saved execution register, shares arithmetic/stack/error
logic with canonical execution, and restores the caller's state. No decode
pointer is stored in the source representation. Canonical package generation
and its Rust/native bytecode contract are unchanged.

### M5 result

The runtime representation is implemented as specified, retaining M4 folding.
Preparation compacts during the existing rewrite rather than adding another
traversal. The shared evaluator directly decodes narrow literals and operator
bytes; canonical scratch is not retained or reconstructed during replay.

Identical-input release comparison against the M4 native snapshot, same 68020 /
2 MiB guest, exact live Rust and independent workload bytes:

| Workload / target | M4 | M5 | Observed change |
|---|---:|---:|---:|
| expression-replay32 / m6502 | 2.244 s | 2.211 s | −1.5% |
| expression-replay32 / m68000 | 4.735 s | 4.746 s | +0.2% |
| expression-layout32 / m6502 | 2.209 s | 2.242 s | +1.5% |
| expression-layout32 / m68000 | 4.770 s | 4.765 s | −0.1% |

These are single observations with 20 ms polling, not evidence of a stable speedup
or slowdown. Totals include preparation and I/O. This compares representation and
decoder changes together, not their isolated contributions. One layout6502
invocation reached the unchanged 60-second limit without a receipt; its bounded
retry completed and supplies the table entry. No deadline was increased.

Separate instrumented expression-layout32 accounting (M4 → M5):

| Target | Expression payload | Packed source records | Preparation / assembly |
|---|---:|---:|---:|
| m6502 | 3,306 → 1,508 B | 6,571 → 4,773 B | 1.64 / 0.38 → 1.64 / 0.36 s |
| m68000 | 3,306 → 1,508 B | 7,275 → 5,477 B | 2.60 / 1.96 → 2.58 / 1.96 s |

Expression bytes shrink **54.4%**, packed records **27.4% / 24.7%**. Compile and
assembly-evaluation counts remain 225 and 514/578. Peak owned allocation remains
53,248 / 270,336 bytes and retained preparation allocation remains 16,384 / 139,264
bytes: capacity slack absorbs this reduction. All owned allocations are freed.
Thus this is a packed-size gain, not a demonstrated reduction in allocated RAM.
Phase observations include accounting overhead and have 20 ms resolution.

Release image grows from 20,224 to 20,692 bytes;
`fnv1a64:dffcd691646a4981`. Linked reservation grows 428 bytes, from 23,300 to
23,728 (18,844 code, 316 data, 4,568 BSS). Stack scratch remains bounded as in M4;
no new persistent data or telemetry record is added. The instrumented image
reserves 24,584 bytes. Both builds complete within the same 2 MiB guest setting.

**Assessment:** worthwhile additional size savings with broadly unchanged total
time and modest decoder/code cost. Keep this reviewable experimental checkpoint;
no further optimization, semantic expansion or production migration follows
automatically. Pre-tokenized files on 8-bit platforms remain a future application,
not a platform-performance claim from these measurements.

Qualification: 30 batched native compact/entry-boundary cases and the existing
414 general evaluator cases pass. The compact batch covers signed widths, mixed
arithmetic, PC/symbol references, malformed/truncated payloads, stack errors,
overflow, rejection of canonical-only opcodes and general wide arithmetic after a
compact call. Source-level folding and width fixtures verify exact byte totals
(54 bytes across ten expressions; 50 across eleven), with live Rust output parity.
The positive source boundary, intermediate-overflow rejection and original
pre-compaction program-capacity rejection also pass. All source/accounting cases
check balanced cleanup; all native cases retain fresh completion/exit/output proof.

Host compact-oracle and workload tests, library Clippy, Rust formatting, workflow
checks, the staged native engineering gate, and explicit formatting of experimental
and scalar harness roots pass. No physical-hardware measurement, general-language
parity or full native self-host run is claimed. Reproduce with the existing M4
commands and a `74bbd35b` native snapshot as the baseline; the current sources
select only the latest compact prepared form.

## M6: preparation-cost attribution

Measurement-only checkpoint. Use gated reusable stage-transition timing to divide
preparation into native package loading/setup, tokenization, numeric name binding
and raw-record writing, expression compilation/folding/compaction, runtime
finalization, and remaining source streaming/control/record retention. Timings
are exclusive, not nested; name each bucket's included work. Rust-side capsule
generation remains outside native preparation. No optimization or language change.

Use the OS E-clock for short scopes; retain existing coarse phase clocks as a
cross-check. Preserve all registers/CCR at macro sites, account for failed setup,
close timer resources on every exit and keep all instrumentation absent when
disabled. Replace the passive telemetry schema coherently rather than retain old
readers. Verify release image identity against M5 and exact output in both modes.

Run unchanged expression-replay32 and expression-layout32 on both targets, same
68020 / 2 MiB guest, same 10/60/150-second limits. Report stage times and entry
counts, their reconciliation with preparation totals, measurement overhead and
limitations. Existing M5 phase measurements supply an overhead reference; they
are not new runs. No full self-host or expanded workload matrix.

Done means a reviewable attribution with a justified next-step recommendation.
If clock failures, probe overhead or inconsistent totals prevent a useful ranking,
report that limitation and revise measurement before optimizing. Selecting or
implementing the next optimization remains a discussion after this checkpoint.

### M6 result — 2026-09-19

Fresh exact-output/native completion checks pass for all four instrumented cases.
Exclusive E-clock stage seconds in the same 68020 / 2 MiB guest:

| Workload | Target | Package setup | Tokenization | Binding / raw records | Expressions | Finalization | Other | Total |
|---|---|---:|---:|---:|---:|---:|---:|---:|
| replay32 | m6502 | 0.018 | 0.875 | 0.406 | 0.201 | 0.013 | 0.277 | 1.790 |
| replay32 | m68000 | 0.044 | 0.963 | 1.065 | 0.208 | 0.159 | 0.295 | 2.733 |
| layout32 | m6502 | 0.018 | 0.881 | 0.407 | 0.203 | 0.014 | 0.279 | 1.801 |
| layout32 | m68000 | 0.044 | 0.966 | 1.063 | 0.212 | 0.161 | 0.298 | 2.744 |

Each run processes 323 lines, with exactly 323 entries each into tokenization,
binding and expression preparation. Package setup and finalization each enter once;
other enters 326 times, including the initial and terminal transitions. E-clock
frequency is 709,379 Hz with no errors. Stage sums differ from coarse preparation
by at most 0.014 s, within its 0.020 s granularity. Timings are single observations,
not physical-hardware measurements or isolated costs with probe overhead removed.

Included work: package setup covers capsule input, workspace allocation and
frontend initialization. Tokenization covers the tokenizer call; binding includes
writer frame setup, name discovery and raw records; expressions include preparation,
compilation, folding and compact lowering. Finalization closes input, releases
scratch, retains/copies the execution prefix and releases the capsule. Other covers
source streaming, control, prepared-record copies/retention and residual transitions.
Host Rust capsule generation is excluded. Timer transitions charge their overhead
across adjacent stages; totals include all active-interval probes.

On layout32, the recorded M5 preparation times were 1.64 / 2.58 s; M6 observes
1.80 / 2.74 s, an increase of 0.16 s (9.8% / 6.2%). Assembly remains approximately
0.36 / 1.94 s versus M5's 0.36 / 1.96 s. This is an overhead estimate against
previous observations, not a fresh controlled calibration. No cost is subtracted
from individual buckets. The broad ranking is clear: tokenization plus binding
accounts for about 72% / 74%; expression preparation about 11% / 8%.

Packed layout32 sizes remain 4,773 / 5,477 bytes, expression payload 1,508 bytes,
and tracked peaks 53,248 / 270,336 bytes. Cleanup balances all owned allocations.
The instrumented image is 22,364 bytes with 25,356 bytes linked reservation;
`fnv1a64:9972a0cf591bec94`. Timer port/request allocations are additional OS-owned
profiling resources, outside these assembler allocation counters, and are released
before telemetry export.

Positive folding and intermediate-overflow rejection checks pass with MEM4,
including owned-memory cleanup. Initial checks exposed local-label shadowing in
new timer cleanup; corrected labels were confirmed in the assembled listing and
both native checks rerun successfully. The two initial post-START timeouts supply
no measurement evidence. No timeout was extended.

Reproduce each instrumented workload with the existing FS-UAE environment:

```sh
python3 scripts/performance/prepared_source_native.py \
  --native-test target/debug/deps/asm-5d01a576d9a2b0d5 \
  --binary-source --binary-only --memory-profile 2m \
  --workload expression-layout --blocks 32 --compare-memory \
  --output /tmp/opforge-m6-layout32-memory
```

Use `expression-replay` and a fresh output directory for the second workload.
Omit `--compare-memory` for release validation. Keep the 10 s post-START,
60 s invocation and 150 s batch limits. Both instrumented batches completed in
under 57 s. Build the test executable for the current checkout before running.

Release validation passes on both targets, with the M5 image unchanged:
20,692 bytes, 23,728 bytes linked reservation, `fnv1a64:dffcd691646a4981`.
Thus disabled instrumentation has no release-image overhead. Fresh layout32
START-to-DONE observations are 2.221 / 4.773 s; these are validation observations,
not an optimization claim. Macro gate/byte-transparency tests, three measurement
script tests, library Clippy, formatting, workflow checks and native engineering
guards pass. Experimental harness formatting covers all 21 linked source files.

### M6 interpretation and proposed next checkpoint

The binding bucket includes raw-record construction as well as lookup; its timing
alone is not proof that every comparison is redundant. Code inspection shows
`binary_frontend.bind` scans the package dictionary, then the session symbol list,
for each name. Tokenization is also substantial. Expression preparation is much
smaller, so direct compact-expression compilation is not the first recommendation.

Propose one bounded follow-up: measure and replace linear name discovery with a
compact indexed binding path shared by all target packages. Preserve normalization,
alias identity, first-use IDs, forward references and rejection behavior. Keep the
reference path during comparison, include index construction in preparation time,
and report temporary allocation and packed/output identity on these same workloads.
Choose the index representation after examining dictionary and symbol distributions;
use offsets in stored data, with no CPU-specific shortcuts. This is a proposal for
discussion, not an activated implementation step. A later tokenizer experiment
should attribute dispatch/scanning work before changing its execution contract.
