# String-free assembly replay experiment

Status: S1 implemented; S1M measures the native baseline and packed-record tradeoffs before expanding semantics.
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
with at most 256 identities, while aliases/size variants can enlarge its current
namespace. Native execution-platform alignment is independent of source target CPU
and emitted data byte order. For Amiga execution, aligned words may be cheaper than
maximum byte packing. Performance takes precedence over minimizing input bytes.

The next inspectable decision is whether to implement a small native reader and
numeric lookup/replay boundary. Do not expand the Rust-only language subset first.
No production format or runtime-package contract is adopted by this probe.

## Next slice: bounded native confirmation

Lower the justified representation to compact native records and reusable telemetry
macros. Compare complete small cases under the existing fresh native proof contract,
with a 60-second invocation and 300-second batch ceiling. Account for executable
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

### Fresh native baseline

Eight S1 blocks contain 16 instructions, 16 labels, eight forward bytes and eight
forward words. Native inputs add `.cpu`, `.org $0100` and `.end` around the same
488-byte body used by the Rust experiment. Both cases use the current 368,635-byte
canonical package (SHA-256 `ae77de3ff1a7966af7f33f89b89062ef4aa5bdb0ce209fedee2e6b3e44826e4a`).

| Source target | Wrapped source | Exact output | Guest START–DONE, telemetry off | Whole invocation |
|---|---:|---:|---:|---:|
| m6502 | 515 B | 40 B | 1.0778 s | 23.22 s |
| m68000 | 516 B | 56 B | 1.0302 s | 23.15 s |

These are single observations, not stable speed ratios. START–DONE is host-observed,
includes input/package/assembly/output and excludes emulator boot. The uninstrumented
native executable is 563,768 bytes (`fnv1a64:106b19ba51b9effc`). Both actual cases
passed Level D: fresh challenge, guest start/completion, explicit zero exit and
byte-for-byte equality with their live in-memory Rust oracle. The native executable
build target is 68020; **the actual emulator is A4000/68040 with 2 MiB chip, 8 MiB
fast and 64 MiB Zorro III RAM**. This is not a 68020/2 MiB feasibility demonstration.

Separate runtime-instrumented runs took 1.0442/1.0303 seconds START–DONE. The
instrumented executable is 567,776 bytes, 4,008 bytes larger. Existing 50 Hz phase
telemetry reports 15 ticks (0.30 s) for each case; this internal interval and the
host protocol interval measure different boundaries. Coarse ticks and host timing
noise do not support claims about the telemetry's timing overhead.

Both instrumented cases report 150 statement visits over 50 statements, 48 encoding
calls, 48 compact lookups and **76,080 strings examined**. TKVM/PRVM/EXVM/ExprVM
operation counts are 1,554/1,401/80/184. CTBL rows examined differ: 528 for m6502,
1,104 for m68000. Peak compact metadata is 4,628 bytes, not total or peak process RAM.
This is concrete evidence for binding names once, but it does not by itself measure
the fraction of runtime removable by doing so.

The [bounded native runner](../../scripts/performance/prepared_source_native.py)
generates the exact sources, checks the live receipt against an independent byte
contract, records package/source/executable/config hashes, and reuses the existing
fresh-proof runner. It caps each invocation at 60 seconds and the batch at 150
seconds; guest timeout is 35 seconds. Build the ASM library test executable first
with `cargo test -p asm --lib --no-run --locked`, then pass the emitted executable:

```sh
python3 scripts/performance/prepared_source_native.py --native-test <asm-test-executable> --profile off
python3 scripts/performance/prepared_source_native.py --native-test <asm-test-executable> --profile runtime
```

Set `OPFORGE_FS_UAE_BIN`, `OPFORGE_FS_UAE_CONFIG_TEMPLATE` and
`OPFORGE_FS_UAE_ARGS` for the installed emulator as described in the
[FS-UAE guide](../../agents/rules/fs-uae.md). Reports default to unique ignored build
directories. Actual guest protocol/output trees remain ephemeral. These initial
observations used the same underlying runner before the convenience script was
added; the new wrapper was checked without rerunning the four completed cases.

There is no native prepared-source candidate yet. The next performance claim needs
a complete small native path that consumes numeric statement/symbol identities and
executes bound package programs, measured against this baseline. A native decoder
alone or Rust replay timing cannot establish that claim. Keep initial parsing/binding
in total time, and show that the same final bytes are produced without per-pass
source/package name lookup before adding instruction-selection coverage.

### Packed layout probe

The [raw block probe](../../crates/opforge-asm/src/prepared_source_experiment/packed.rs)
uses one owned byte block and borrowed readers. Its 12-byte header carries record
and symbol counts plus layout options. Each line has a u16 byte-length prefix,
u16 source line, flags and only its present fields. Labels use u16 identities;
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
| Byte packed, byte instruction IDs | 540 B | 2,124 B |
| Byte packed, word instruction IDs | 556 B | 2,188 B |
| Word aligned, byte instruction IDs | 604 B | 2,380 B |
| Word aligned, word instruction IDs | 604 B | 2,380 B |

Both assembly targets have identical packed source payloads. Output endian rules
and instruction program bytes are external. Counts include header, lengths, source
locations and padding, but exclude the instruction dispatch map, encoding directory,
output, workspace and allocator overhead. The probe converts an existing typed S1
object and keeps it during validation: **this is not a measured pipeline RAM saving
or a direct packed-tokenizer implementation**. There is no compressed-string or
comment pool. This comment-free, short-line fixture offers no comment-removal saving.

The byte-packed form is close to text size, but does not yet beat it. Word alignment
costs more bytes; using word IDs then costs no additional space because it replaces
padding. Avoid choosing byte IDs solely for their nominal width on a word-oriented
execution platform. Reducing per-line metadata or combining label-only records may
save more, but is deferred until native read/dispatch costs can guide the decision.

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

For 32 blocks on the m6502 source, byte-packed byte/word-ID decoding took
7.58/7.28 us per scan; word-aligned byte/word-ID decoding took 7.82/7.69 us.
Packing took 5.17–7.08 us across those layouts. These are arm64 observations,
not native cycle estimates; the checksum and bounds checks are included. Small
host differences do not select a native winner. The identical m68000 source records
produce the same checksums and sizes.

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
