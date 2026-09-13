# Focused package-VM efficiency measurements

Run from the repository root on a POSIX host:

```sh
python3 scripts/performance/vm_efficiency.py selection --blocks 8,32,128
python3 -m unittest discover -s scripts/performance/tests -p test_vm_efficiency.py
```

The runner builds the release CLI and package generator once with
`vm-runtime-only`, generates a canonical package and supplies it explicitly to
every invocation. Build/setup has a separate ten-minute build cap and is reported
separately; it is not a measurement sample. Measurements have a 60-second
invocation cap and one five-minute batch deadline across all families. Timeout
terminates the owned process group, fails the run and leaves an incomplete summary.
It never launches native self-hosting or an emulator, extends a deadline, or
automatically retries a timeout. Smaller block counts can be selected explicitly.

Each workload runs in two tokenizer modes using the same executable, canonical
package and source: automatic specialization and forced-generic execution. The
runner sets `OPFORGE_TOKENIZER_FORCE_GENERIC=1` only for the latter, and removes
ambient opForge overrides. This switch disables the default-dispatch tokenizer
specialization; it does not disable every Rust helper or specialization elsewhere.
Attribution must demonstrate the fast path in automatic mode and real tokenizer
bytecode dispatch with no fast-path calls in generic mode. Every output is checked
against the independent byte contract; the two modes must also have identical
source/output hashes, negative diagnostics and successful tokenizer step totals
(fast logical budget steps versus generic dispatch). All eighteen positive cases
and six negative companions share one five-minute deadline. `tokenizer_comparisons`
in the summary records each pair; timing differences remain descriptive.


Default output goes to a new ignored `build/vm-efficiency-*` directory. Use
`--output <new-directory>` to choose another location; existing directories are
rejected. Each case retains `input.asm`, independently calculated `expected.bin`
and the actual `output.bin`. Every size also retains `profile.txt`; negative
cases retain their diagnostics. `summary.json` includes build features/profile,
binary/package/source hashes, source revision and tracked-diff identity, compiler,
build environment, raw timing samples, profile rows, VM work aggregates and limitations. Results are
local inspection artifacts, not a committed historical ledger.

## What the workload proves

Three variants exercise shared selection, operand, expression and pass mechanisms:
MOS 6502, Z80 and Motorola 68000. Each block mixes immediate loads, a memory store,
a local forward branch, data and a symbol-derived address. Distances stay bounded
as block count increases. Seven emitting statements and a separate block label
produce 13 bytes per block for 6502/Z80 and 18 for 68000. Instruction bytes and
address fields are calculated from the fixture's ISA contract independently of
opForge. This is a focused contract, not an external-oracle claim for entire ISAs.

The current VM-only package rejects Zilog `LD r,n`; the Z80 case explicitly uses
supported Intel-compatible `MVI r,n`, while retaining Z80 branch and memory forms.
That uncovered dialect gap is not fixed or hidden as a benchmark optimization.
Each family also checks an undefined-symbol diagnostic and nonzero exit; a signal
is not an acceptable negative result.

The build's `vm-runtime-only` feature removes the host instruction-encoder fallback
in `AsmLine::process_instruction_ast`. Profile validation additionally requires
positive `vm.parse` and `vm.encode` counts and rejects host parse/encode labels.
This proves the measured parser/encoder route, not that every directive or other
assembly operation is implemented by bytecode. Profiling counters include failed
encoding attempts. `vm.model.bootstrap` measures the assembler runtime-model
bootstrap wrapper while profiling is active; it is not a count of every model
construction elsewhere in the process.

## Target-callback boundary probe

```sh
python3 scripts/performance/package_boundaries.py --blocks 8,32,128
python3 -m unittest discover -s scripts/performance/tests -p test_package_boundaries.py
```

This companion runner uses the same build, canonical package, inputs and deadlines.
It compares baseline with `OPFORGE_TARGET_CALLBACKS=report`, then runs `refuse` on
identical inputs. Report mode preserves execution; refuse mode rejects an attempted
call before invoking it. The engine accepts the environment switch only with a
`vm-runtime-only` build and VM execution; invalid values or other routes fail.
Library probes can install a scoped `types::target_callbacks` session explicitly.

The four instrumented boundaries are registered family candidate resolvers,
registered family parsers for compound instruction operands, the inline family indexed-postfix parser,
and the family operand-plan fallback. These are conservative boundaries: even a
helper that declines the input or internally uses package data counts as an attempt.
They do not inventory every target-specific rule or every Rust helper in the code.
No reported callbacks is therefore not proof of wholly package-controlled execution.

`[opforge target callbacks]` emits one JSON report per assembly, including boundary,
family, CPU, operation detail, attempt count and first refusal. Refusal stays fatal
through candidate/parser recovery and is checked before binary publication. A
pass-two-only refusal may leave diagnostic listing output, but cannot produce a
successful assembly. Reports cap distinct keys at 256 and mark overflow incomplete;
these diagnostic allocations are not a target-memory estimate. Existing VM-work
reports separately count callback attempts/refusals by phase, descriptor work,
bytecode dispatch and tokenizer specialization. Callback counts are not VM steps
or successful-handling counts, and do not measure time inside helpers.

The runner checks 18 successful baseline/report assemblies against independent
bytes, nine strict attempts, and 18 negative companions for identical undefined-symbol
diagnostics. A strict attempt must either exit 1 without a binary after encountering
a callback, or complete callback-free with the independently expected bytes. A
callback-free NOP control must still succeed in refuse mode; it selects its CPU
through the CLI to isolate instruction emission. Profiles
are excluded from diagnostic comparison; actual diagnostics remain compared.
Artifacts and `summary.json` go to a fresh ignored `build/package-boundaries-*`
directory. Interrupted/failed batches remain incomplete. The fixed run order and
instrumentation make this a boundary classification, not a speed qualification.

## Comparing operand routing changes

Preserve the baseline executable and its canonical package in ignored build output
before rebuilding. Capture its results, then compare the candidate on the same
source cases. Supply explicit paths so an old summary cannot silently use a newer
executable for timing:

```sh
python3 scripts/performance/operand_routing.py \
  --reference-binary build/b2-reference/opforge \
  --reference-package build/b2-reference/runtime.opasm \
  --output build/operand-reference
python3 scripts/performance/operand_routing.py \
  --reference-binary build/b2-reference/opforge \
  --reference-package build/b2-reference/runtime.opasm \
  --reference-summary build/operand-reference/summary.json \
  --candidate-binary target/release/opforge \
  --candidate-package build/b2-candidate-build/runtime.opasm
```

The supplied binaries must be VM-only builds; normal report-mode invocation
rejects an unsupported build or execution mode. The runner compares nine scaling
cases and twelve focused directive, expression and addressing cases. It keeps
output bytes, errors/spans and callback attempts separate. A diagnostic difference
returns a nonzero result and is recorded even when both runs reject invalid input;
review any intentional change explicitly. No fixture-specific waivers are applied.
A timeout preserves an incomplete summary. Each stage uses the same 60-second
invocation and five-minute batch limits; building the binaries is separate setup.

Three paired, unprofiled samples per side on the 32-block cases include process
startup and model preparation; warmups are excluded. These short samples describe
the observed cost and do not qualify a speedup. The B2 change removes family
consultation for directive grammar and shared instruction atoms. Its 68000
32-block callback count is 32, down from 226; 6502/Z80 candidate-resolver counts
remain 320. VM dispatch totals are unchanged. Complex addressing hooks remain.
Temporary executable/package oracles and result directories are not committed.

## VM work and repetition

Attribution enables `OPFORGE_PROFILE_VM_WORK=1`. The existing assembly-profile
lifetime owns a thread-local collector, also usable independently of timing
profiling. Ordinary timing runs disable collection; dormant hooks remain compiled
in. The profiler has no native implementation and its own memory is not a native
footprint estimate.

`summary.json` contains `vm_work` for each case: exact program bytes, engine and
version; per-phase invocation counts; executed-position/opcode histograms; and
aggregate ratios and the ten programs doing the most work. IDs are local to a
report; use engine, version and bytecode identity for comparisons across reports.
Exact bytes distinguish programs internally; the SHA-256 in the ranking is a
convenient external identifier. Identical bytecode with different bindings shares
an identity, so this does not prove equal semantic inputs.

- `bytecode_steps` counts dispatched operations in the statement parser, expression
  parser/evaluator, interpreted tokenizer, value, basic emission, selector and
  operand-record executors. Operand bytes are not instructions. An operation that
  subsequently fails still counts. Terminators count when actually dispatched;
  terminators consumed solely by validation/decoding do not.
- `decoded_steps` counts executed encoding, structured-encoding, fixup and branch
  operations. Engines ending in `.steps` use decoded-step ordinals rather than
  byte offsets. `dispatch_steps` is the sum of both categories, not equivalent
  hardware instructions or uniform-cost units.
- `repeated_within_call` counts subsequent visits to an executed position/opcode
  during the same invocation. `repeated_between_calls` counts repeated visits
  across invocations after subtracting within-call repeats and first visits.
  Neither implies unnecessary work: operands, bindings and state are not compared.
- Separate events cover descriptor-selector attempts/results, basic emission's
  operand bytes copied, expression instability scans and the tokenizer fast path.
  Fast tokenizer logical budget steps are **not executed bytecode** and never enter
  dispatch totals. Input bytes describe the supplied input, not all memory reads.
  Descriptor-selector events cover that specific route, not every target's
  candidate mechanism; their absence is not evidence of zero selection cost.

Direct fast/generic equivalence tests compare full tokens (including spans) and
exact error strings across three families, explicit edge cases and deterministic
fuzz inputs. Reduced step/token/lexeme budgets exercise failure correspondence.
These tests qualify current coverage; they are not a proof for all possible
inputs or policy combinations. The fast path remains hand maintained and is
eligible only for its recognized program structure/bytes. Any native feasibility
argument must distinguish this specialization from forced-generic execution.

Ratios divide complete-case execution by five assembled instructions per block,
all source lines, or source bytes. The workload also contains two data statements
per block and labels, so the instruction ratio includes more than instruction
encoding. Counters exclude package validation/decoding, declarative state-table
operations and other Rust helper internals; a coarse opcode may hide substantial
work. No claim of complete machine work, cache behavior, native throughput or
identical-input redundancy follows from these numbers.

Aggregate collection retains no event trace. It caps distinct program bytes at
4 MiB, programs at 4,096 and histogram positions across rows at 200,000. Overflow
marks the report incomplete and fails the comparison. Tests check count
conservation, repeated invocations versus loops, session isolation and actual
emitter dispatch through success and failure. All workload sizes retain the same
60-second invocation and common five-minute batch limits.

## Interpreting the result

Each size/family gets one warmup and three uninstrumented timing runs; each family
gets one separate attribution run at every size. Profiling percentages and nested
rows overlap: do not sum them indiscriminately or compare their timings directly
with uninstrumented samples. Timing is fresh-process, fixed-order and subject to
host activity. Small sample ranges are descriptive, not confidence intervals.
Repeated agreement is supplemented by independent byte checks on every run.

Model/package setup can dominate these small complete assemblies. Compare size
slopes and per-family results before treating a repeated operation as a worthwhile
optimization. Compare future candidates on identical inputs/settings. No result
here predicts full-self-host duration or proves a native speedup.

Memory reporting is deliberately limited: file sizes are measured; process peak
memory and native working memory are not. The summary includes a labelled
sensitivity calculation for 8/16/32-byte per-statement records alongside source,
output and canonical-package storage. It excludes code, symbols, decoded tables,
scratch and the OS, so it is neither a peak estimate nor proof of fitting 2 MB.
The next representation experiment must account for those missing terms.

The current work and brief decision are tracked in the
[reset plan](../plans/native-runtime-reset.md), not in this tool guide.

## Minimal Rust/native comparison

Build the current release CLI with `vm-runtime-only` and the assembler test
executable separately (ten-minute preparation cap). Cargo's JSON
`compiler-artifact.executable` identifies the actual test executable:

```sh
cargo build --release --locked -p cli --bin opforge --features vm-runtime-only
cargo test -p asm --lib --locked --no-run --message-format=json
```

With the FS-UAE environment from the [emulator guide](../../agents/rules/fs-uae.md), run:

```sh
python3 scripts/performance/runtime_comparison.py \
  --rust-binary target/release/opforge \
  --native-test <assembler-test-executable> \
  --output build/runtime-comparison-new
```

This deliberately small comparison reuses the mixed instruction/data generator
at 8 and 32 blocks, for m6502 and m68020 source targets. Use
`--cpus m68020 --blocks 16` for a smaller complete case when the larger one
cannot finish within the deadline. Both CLIs start with
m6502; the identical source selects the actual target. Both Rust tokenizer modes
get a warmup, three unprofiled samples and a separate VM-work attribution run.
Forced-generic affects the tokenizer only. Each native case runs once, unprofiled,
through the existing fresh-challenge, zero-exit, exact-artifact proof runner
(Level D). Its oracle is assembled in memory from the actual source and checked
against independent workload bytes. The supplied package must equal the live
Rust registry package. This is output equivalence for these cases, not evidence
that both implementations execute identical VM programs or dispatch counts.

The batch cap is five minutes, each invocation at most 60 seconds, and the emulator
wait (including boot) and post-start wait are capped at 35 seconds within that
invocation budget.
No self-host case or timeout retry is included. Native failures are recorded and
later cases still run while budget remains. Owned guest run trees are ephemeral;
local inputs, timing receipts and summaries remain under ignored `build/`.

Rust timing covers a fresh host CLI process. Native timing is the host-observed
interval from guest START to DONE, with 20 ms polling; it excludes emulator boot
but includes command loading, package processing, assembly, output and protocol
overhead. These are descriptive end-to-end observations, not comparable hardware
cycles or calibrated speed ratios. The summary records the template and the
runner's effective 64 MiB Zorro III memory override. Neither this emulator setup
nor package file size establishes 68020/2 MB feasibility. Peak RAM and native
VM-work counts are unmeasured. Current findings and the next decision belong in
the [reset plan](../plans/native-runtime-reset.md).

For a native before/after comparison, use `--blocks 8` and pass
`--native-source-root <snapshot-root>` for the baseline. The snapshot must contain
its `native/` tree; both runs still use identical current benchmark source and
explicit package bytes. The summary records the actual native source-tree hash,
linked executable bytes and digest. Compare the source/package/output hashes
before interpreting timings. Executable file size excludes BSS and allocated RAM.

Add `--native-profile runtime` for VM/service/candidate work and prepared-CTBL
counters: successful preparations, lookups, strings reconstructed, program entries
prepared, binary-search rows examined, and peak program-directory allocation bytes.
The last counter excludes fixed state and allocator overhead.
Those samples are instrumented: compare their work counts with unprofiled timing,
not as release-speed measurements. The decoder requires complete, correlated,
non-overflowing records from the same fresh successful guest. `--native-profile all`
selects all existing counter families for explicit diagnosis; that combination
failed the initial mixed-workload checks during expression compilation and has
not been requalified after the macro conversion; it is not valid attribution evidence. It must not be silently substituted for release behavior.
