# Binary-source native runtime

Status: F1 named constants implemented; current design and M8 baseline below.
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
- The current BSP2 capsule is an experimental host-to-native vehicle. It is not a
  persistent source-file or runtime-package format. Producer and consumer move
  together; BSP1 is rejected and no compatibility executor is retained.

The design intentionally leaves room for an editable tokenized source format on
constrained platforms. Such a format would need optional spelling/format metadata
and a stable vocabulary identity, but its execution records would still use
offsets and numeric IDs. No disk-format contract has been adopted.

## Current implementation coverage

The current path supports one selected package pipeline; package-defined
registers and immediate operands; parenthesized member operands; colon labels; forward branches;
the program counter; parentheses; unary `+`/`-`; checked signed addition,
subtraction and multiplication; and shared `.cpu`, `.org`, `.byte`, `.word`,
`.long` and `.end` directives. It performs a fixed two-pass layout and emission
over contiguous output.

F1 adds the bounded `name = expression` form. An immutable constant is resolved
when its definition is reached in pass one and must evaluate to the same value in
pass two. It may use earlier constants, earlier labels and the current program
counter. Forward and deferred constant dependencies reject rather than being
silently bound to a provisional value. This is deliberately narrower than the
complete Rust language contract; later expression/dependency work will be selected
from representative cases. The compact evaluator also preserves signed symbol
values; this correction is currently limited to the compact path.

Other limits remain explicit:

- no files/includes, modules, scopes, macros, conditionals, loops, structs or lists;
- no strings, general sections, relocations, relaxation or complete expression
  operator set;
- no discontiguous `.org` after output has started;
- literals are limited to the currently checked signed 32-bit subset, and labels
  cannot reuse reserved package spellings;
- maximums include 64 tokenizer tokens per line, 512 source names, 4 KiB textual
  line, 256-byte packed line and 1 MiB per growing allocation;
- diagnostics are provisional and the normal native CLI is not qualified through
  this route.

The host builds the current BSP2 package capsule and passes it with raw source to
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

## F1 breadth checkpoint

F1 is implemented: named constants in a purpose-written 6502 byte-reversal
routine (`reverse-byte.asm`, 10 bytes of code, using zero-page destructive scratch)
and a 68000 range-check routine, plus generic data cases for negative constants,
chains, label differences and program-counter references. These are standalone
representative routines, not full existing applications. The tokenizer must emit
the binary form directly and assembly must not recover text or string spellings.
Success requires exact live-Rust/native output for both routines, focused constant/data contracts
and the existing bounded workload as a regression. Unsupported dependencies reject
explicitly. Stop if the slice requires a general dependency resolver, new textual
lookup or another owned preparation buffer. Measure the complete bounded cases and
representation cost, then review the F2 package-owned operand-shape increment in the
[migration plan](native-runtime-reset.md#breadth-migration-plan).

The original 6502 page-copy candidate remains an explicit expected native
rejection. Its indexed operands require `direct_x`/`direct_y` recognition supplied
by the Rust family parser, while the capsule lacks equivalent package-owned
structural and register predicates. Native must not accept an unchecked token pair
or add a CPU-specific shortcut to generic code. F2 should carry those predicates
through the package/capsule boundary and prove equivalent selection.

Negative constants exposed an unsigned symbol-load error in the shared evaluator;
F1 corrects it only for compact execution. Cyclic constants expose a separate Rust
reference gap: the current 6502 path accepts them through provisional zeros and
pass-two updates, whereas native rejects them. Leave that behavior unresolved in
F1 and address explicit cycle/deferred-dependency semantics with the later
expression resolver work.

The register-copy case `move.l d0,d1` also rejects: a higher-priority package
rejection predicate is not yet executable in this view. Merely enabling the
register-pair shape or skipping unsupported candidates is insufficient. F2 must
carry the predicate semantics before claiming that coverage. The F1 range-check
routine instead calculates and returns directly in D0; it is 22 bytes of code.


### F1 validation and reproduction

Fresh FS-UAE runs on the same 68020 / 2 MiB profile matched live Rust bytes for
both complete routines and the generic arithmetic case. The instrumented cases
released all tracked allocations, with no profiling errors. Seven separate native
rejection cases passed: forward dependency, cycle, duplicate constant,
label/constant collision in each order, missing name and trailing expression
syntax. Page-copy and register-copy cases passed as explicit subset rejections,
not successful assembly or diagnostic parity.

The 42-case native compact-expression batch includes twelve new alternating
compact/canonical calls checking signed symbol boundaries and preserving the
canonical unsigned table contract. Focused host oracles, package relocation
checks, library Clippy, Rust/native formatting, workflow checks and staged native
engineering guards passed. No full native CLI or full-project qualification is
claimed.

| Instrumented routine | Source bytes | Packed bytes | Expression bytes | Compiles / evaluations | Retained after preparation | Peak owned |
|---|---:|---:|---:|---:|---:|---:|
| 6502 byte reversal | 571 | 243 | 53 | 12 / 24 | 8,448 B | 73,984 B |
| 68000 range check | 526 | 297 | 67 | 14 / 28 | 131,584 B | 262,656 B |

The source sizes include comments and formatting. This is representation and
assembly evidence; these routines were not executed on their target CPUs.
Constant support adds no owned buffer or per-symbol storage. Definitions reuse
the existing values/defined arrays and compact-expression work counters.

Run host contracts with `cargo test -p asm binary_constants -- --nocapture`.
For native proof, use the known-good invocation environment from the emulator
guide, set
`OPFORGE_FS_UAE_SMOKE=1`, `OPFORGE_FS_UAE_MEMORY_PROFILE=2m`,
`OPFORGE_FS_UAE_TIMEOUT_MS=60000`, `OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=10000`,
`OPFORGE_FS_UAE_POLL_MS=20`, and run individual tests:

```sh
OPFORGE_COMPARE_MEMORY=1 cargo test -p asm binary_constants_reverse_byte_fs_uae -- --ignored --nocapture --test-threads=1
OPFORGE_COMPARE_MEMORY=1 cargo test -p asm binary_constants_range_check_fs_uae -- --ignored --nocapture --test-threads=1
OPFORGE_COMPARE_MEMORY=1 OPFORGE_CONSTANT_REJECTION=cycle cargo test -p asm binary_constants_rejection_fs_uae -- --ignored --nocapture --test-threads=1
```

The other rejection selectors are `forward`, `duplicate`, `label_collision`,
`constant_collision`, `missing` and `trailing`. Run the ignored arithmetic and
operand-gap tests individually by their names in
[binary_source_constants.rs](../../crates/opforge-asm/src/tests/binary_source_constants.rs).
`native_expression_compact_fs_uae` runs the compact/canonical entry batch without
`--ignored`. Keep invocations serialized and enforce the 150-second outer batch
bound when grouping them. Set `OPFORGE_COMPARE_MEMORY=0` for release routine timing.


### F1 release regression comparison

One fresh matched `expression-layout32` observation per target, comparing M8
`0c560e98` with F1, measured **1.934 → 1.921 seconds** for m6502 and
**3.823 → 3.840 seconds** for m68000. Outputs matched live Rust in all four runs;
package capsules were unchanged. These differences are within the 20 ms polling
interval and ordinary run variation: no large regression was observed, and this
is not evidence of a speed improvement or a precise performance bound.

The release image grew **20,988 → 21,176 bytes** (+188 B, about 0.9%); linked
reservation grew **24,020 → 24,200 bytes** (+180 B). F1 image identity is
`fnv1a64:f1c6d37284799a4c`. Both measurements use the uninstrumented image.
The baseline used M8 native sources and its unchanged capsule producer, while F1
used the final current sources. Use the reproduction command above with each
revision's matching test executable and native tree for a fresh comparison.

Fresh uninstrumented routine observations were **0.326 seconds** for byte
reversal and **0.623 seconds** for range checking, using that same release
image. These small cases include I/O and program/protocol overhead and are
coverage measurements, not throughput benchmarks.
