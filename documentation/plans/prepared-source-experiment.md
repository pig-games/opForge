# String-free assembly replay experiment

Status: S1 implemented and measured; ready for review as an experimental checkpoint.
The active
[AGENTS.md](../../AGENTS.md) and [workflow](../workflow/README.md) remain binding.

This plan uses the local [plan-authoring skill](../../skills/opforge-plan-authoring/SKILL.md).

## Hypothesis and contract

Resolve source and package names during initial preparation, then perform repeated
assembly work using compact identities and structured values. The ideal is zero
string lookup after preparation and little retained original text. Mnemonic IDs
identify operations, not machine opcodes. Package semantics remain authoritative;
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

## S2: prepare value-dependent instruction work

Use S1 evidence to select one coherent operand/selector preparation boundary.
Preserve package-defined choice rules and bind expression symbols by identity;
reevaluate values, candidate eligibility and relaxation without spelling lookup.
Include scope/macro-instance identity and changing CPU/dialect state when expanding
coverage. Do not install a generic cache keyed only by source line or mnemonic.
Connect the existing portable expression machinery to numeric symbol references;
do not grow the laboratory's small expression evaluator into another complete
expression implementation. Choose a compact storage layout from measured costs,
not by translating Rust enum layouts directly into native records.

## S3: bounded native confirmation

Lower the justified representation to compact native records and reusable telemetry
macros. Compare complete small cases under the existing fresh native proof contract,
with a 60-second invocation and 300-second batch ceiling. Account for executable
size, preparation peak, retained RAM and total time. No long-running self-host test.
Delete superseded responsibilities on integration instead of retaining permanent
parallel pipelines. Review before expanding into a general native migration.

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
package-defined form-selection boundary belongs to S2, without CPU-specific cases
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

An isolated single-thread measurement ran in 1.12 seconds, without telemetry,
on arm64 with Rust 1.95.0 and the repository's opt-level-1 test profile. Each block
contains two operandless instructions, two labels, a relative forward byte and an
absolute forward word. Every measured case matches its live ordinary-assembler
oracle. The 32-block source has 64 instructions and 192 retained source records.

| Target / blocks | Input text | Frozen object | Replay workspace | Source preparation | 16 full replays |
|---|---:|---:|---:|---:|---:|
| m6502 / 8 | 488 B | 2,411 B | 152 B | 88.4 us | 23.5 us |
| m68000 / 8 | 488 B | 2,413 B | 152 B | 51.5 us | 29.3 us |
| m6502 / 32 | 2,062 B | 9,131 B | 536 B | 151.2 us | 97.5 us |
| m68000 / 32 | 2,062 B | 9,133 B | 536 B | 152.5 us | 105.0 us |

The 32-block object contains 5,912 bytes of statement/operation/operand records and
3,072 bytes of expression records. Its single encoding directory costs 59/61 bytes
including its object header; the frozen totals also account for the source object
without double-counting that header. Workspace covers symbol/stack payloads only.
Allocator overhead, output buffers, call stacks and preparation peak are excluded.
These are Rust layouts, not proposed native layouts or measured process peak RAM.
The old assembler's retained working set was not measured, so no RAM saving over it
has been demonstrated; even the compactness relative to plain source is not achieved.

Package-model construction still costs 133.2–135.6 ms in these samples and must be
added to source preparation and replay. Ordinary complete assembly costs
142.6–150.6 ms including its setup. These paths have different setup responsibilities;
the small warm replay intervals are not a comparable CLI speedup or native estimate.
The initial combined correctness/measurement run took 1.17 seconds; the isolated
measurement above removes competing test execution. Neither run uses an emulator.

Reproduce the uninstrumented measurement separately from counter checks:

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
for preparation peak. The next semantic extension should carry IDs into one
value-dependent package operand/selection route rather than add more operandless
cases or grow a second general expression evaluator.

Validation: all 423 VM library tests pass with telemetry enabled; the focused
disabled-telemetry VM tests also pass. The complete source/regression/measurement
run passes all eight selected tests; the separate instrumented run passes seven
and leaves the manual benchmark ignored. Four existing data-evaluation and
unknown-symbol tests pass. Production ASM/VM Clippy with warnings denied, formatting,
the workflow gate (136 tests), links and CPU-boundary guard pass. Expanding Clippy
to the ASM test target finds four pre-existing warnings in untouched native harness
files (large enum, two manual range patterns, one unnecessary vector); that broader
check is not green. No full-workspace or native qualification is claimed.
