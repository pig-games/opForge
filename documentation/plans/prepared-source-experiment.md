# Binary-source native runtime

Status: F2 is the last qualified checkpoint; its design and measurements are below.
F3 work is in progress with a known Rust dependency defect; see the
[F3 checkpoint](native-runtime-reset.md#f3-checkpoint-reference-defect-and-scope-decision).
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
registers, register pairs, indexed and immediate operands; parenthesized member operands; colon labels; forward branches;
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

The host builds the current BSP3 package capsule and passes it with raw source to
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

## Current breadth checkpoint: F2

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
rather than select a provisional short width and move later labels. The existing
Rust cyclic-constant gap is
still recorded for the next expression increment; native rejects those cycles.
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
