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
[completion plan](../plans/native-runtime-reset.md), not in this tool guide.

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
the [completion plan](../plans/native-runtime-reset.md).

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


## Current composed-binding control measurements

The release controls at `fd6e848f` use identical 11,592-byte source with 1,200
macro invocations, both embedded packages and FS-UAE 68020/10 MiB. Every sample
checks the entire output against live Rust. Instrumentation is excluded.

| State | START-to-DONE seconds | Image bytes | Linked reservation bytes |
|---|---:|---:|---:|
| Preview-isolated baseline | 26.894099 | 646,244 | 670,620 |
| Capture ordering | 26.860895 | 646,860 | 671,212 |
| Inactive generated-record isolation | 26.907188 | 646,920 | 671,272 |

One sample per state supports no meaningful timing difference. The overall image
grows 676 bytes and linked static reservation grows 652 bytes. Static reservation
excludes runtime heap; peak allocation and 2 MiB fit were not measured. A sample
that overlapped a host build was discarded and repeated. Focused behavior and
the subsequently repaired active-member behavior belong in the
[frontend boundary](../plans/compact-frontend-vm-boundary.md#current-opcore-breadth-and-acquisition-boundary).

The first numeric proxy-owner repair uses the same unchanged control and
profile. A fresh baseline takes 26.936725 seconds; the repaired image takes
26.910136 seconds. One sample per state supports no meaningful timing difference.
The repair adds 500 image bytes and 480 linked reservation bytes, reaching
647,420 and 671,752 respectively; it reuses the existing owner-index allocation.
The repaired image SHA-256 is
`7137c1c147b58b85c06826a5542811388f55eefe08f5e3174e0018783e0b8df6`.
Twelve positive member-binding controls and two malformed-name rejection controls
pass freshly. These are focused correctness results, not a full self-host or
general compound-ownership qualification. Runtime heap peak remains unmeasured.

The conservative owner policy adds forward-field and mutable-kind controls. A
fresh uncontended pair on the same unchanged release control takes 26.873247
seconds before and 26.896839 seconds after (+0.088%, within single-sample noise).
The current image is 657,484 bytes with 681,816 bytes linked static reservation,
SHA-256 `8bdcba82d219e3d392d6968eb05ec0e138267f8cad6527968a99e6cd7b6c09c0`.
Of the 10,064-byte increase, 128 bytes are code and 9,936 bytes are regenerated
package data, including the separate 6502 force-suffix diagnostic repair. BSS is
unchanged; the owner policy adds no heap allocation. Eighteen positive controls
and two malformed-name rejection controls pass freshly across 68020 and 6502.
Different possible physical owners remain an explicit reachable-use rejection
until selection can run against the actual assembly state. These focused results
are not a current-source full self-host or peak-memory qualification.

The following acquired-collection checkpoint uses the same 11,592-byte release
control (1,200 macro calls), FS-UAE 68020/10 MiB and telemetry disabled. With host
builds paused, a fresh paired control takes 26.817550 seconds before and
26.851802 seconds after (+0.13%, within single-sample noise); both complete and
match the entire live Rust output. This measures unchanged-path overhead, not
speedup of a formerly rejected collection case. The final image is 659,608 bytes,
linked static reservation 684,524 bytes, SHA-256
`831e4c1c8700d5ca3ac22a326cc49251285c58ea32d1f4248574303f7b0e2cdc`.
Against the conservative-owner checkpoint this adds 1,984 code bytes and 724 BSS
bytes, with unchanged package data. Twelve fresh exact collection controls pass
across both packages. Peak runtime heap and a new full self-host remain unmeasured.

The frozen-input assembly lifecycle uses the same unchanged release control and
memory profile. A fresh pair with host builds paused takes 26.651326 seconds
before and 26.853994 seconds after (+0.76%, one sample); both match the complete
live Rust output. The attribution build excludes the concurrent packed-text PRVM
dispatch, using a separate source snapshot with the checkpoint dispatcher.
Its image is 660,760 bytes and linked static reservation is 685,476 bytes,
SHA-256 `9d1206f0d9fbdb120c482bc29188f9e6bfb5c424559a36d17ebe317fab34fedc`.
Relative to the collection checkpoint, this adds 888 code bytes and 64 BSS bytes,
with unchanged package data. This measures lifecycle overhead on frozen input;
it does not measure growing-source acquisition, peak heap or full self-hosting.

The independent symbol-storage checkpoint uses the same 11,592-byte control,
1,200 macro calls, FS-UAE 68020/10 MiB and disabled telemetry. Host builds were
paused for the fresh pair: 27.064863 seconds before and 26.944213 after (-0.45%,
one sample), with complete live Rust equality in both runs. This attribution
baseline already includes the packed-text PRVM dispatch; package data is
identical. The new image is 662,904 bytes, linked static reservation 687,660,
SHA-256 `344bd882beac24844abe1353102d9f5ceea85499c2066d5428837d4eeb9ff55a`.
Compared with that 662,380-byte combined baseline, this adds 536 code bytes and
32 BSS bytes (568 linked bytes). Twelve positive and two malformed negative
cases pass freshly for both the production image and the allocation-failure
probe. This measures unchanged-path storage overhead, not growing-source
acquisition, peak heap or a new full self-host run.

## Shared byte reservations (BS37)

The shared `.ds`/BSS slice uses the unchanged 11,592-byte source with 1,200 macro
calls, both embedded packages and FS-UAE `68020-10m`. Both release samples complete
freshly and match the entire live Rust output; telemetry is disabled. The baseline
is the final conditional-owner image, before this reservation change.

| State | START-to-DONE seconds | Image bytes | Linked reservation bytes |
|---|---:|---:|---:|
| Conditional-owner baseline | 33.094098333 | 674,344 | 707,612 |
| Shared reservations | 32.955889917 | 674,768 | 708,008 |

The difference is -0.42% from one sample per state, supporting no material timing
change. The executable grows 424 bytes and linked storage grows 396 bytes. The
final release SHA-256 is
`12e51f1f0eb0b2a506cb03c4c4f4fa9f6363a434056de1651a3c0f8f315ea72a`.
This measures unchanged-path overhead, not a speedup of previously rejected BSS.

A separate instrumented run of the same source matches the full Rust artifact.
Owned-memory peak rises from 1,282,328 to 1,282,344 bytes (+16); allocated and freed
totals both rise from 3,182,104 to 3,182,120. Terminal live bytes remain zero. Packed
source stays 27,022 bytes; runtime prefix changes from 425,504 to 425,506. The
instrumented 34.231945625-second result is not release timing. These measurements
do not qualify physical A6000 speed, full current-source self-hosting or 2 MiB fit.

## Executed declaration activity and scalar observations

This repair separates executed immutable owners from value availability and
retains private numeric layout observations across bounded sizing retries.
Measurements use the same 11,592-byte/1,200-call control, both embedded packages,
FS-UAE `68020-10m`, and disabled telemetry. Host builds were paused for the final
release sample. Each timed run completes freshly and matches the live Rust output.

| State | START-to-DONE seconds | Image bytes | Linked reservation bytes |
|---|---:|---:|---:|
| BS37 reservation baseline | 32.955889917 | 674,768 | 708,008 |
| Execution activity, before scalar observations | 33.160440833 | 676,792 | 709,968 |
| Execution activity plus scalar observations | 33.291479875 | 677,860 | 710,968 |

Activity alone adds 0.62%; the subsequent scalar-observation change adds 0.40%
relative to that intermediate state. The combined difference is +1.02% over
BS37. One sample per state supports no material regression, not a precise speed
claim. Final executable and linked storage grow 3,092 and 2,960 bytes over BS37.
Release SHA-256:
`a4323bcf85b64ce761d10c8b928a74b10c75175c4fc1d16d80f4205118c355ca`.
The new gated work counters leave this release image byte-identical.
The optional labeled-conditional PRVM classifier change was excluded from this
frozen source build; its consumer integration and costs remain a separate slice.

Separate instrumented unchanged-control proof reports peak owned memory 1,282,360
bytes (+16 over BS37), allocated/freed 3,182,136 bytes and zero live bytes. Packed
source remains 27,022 bytes; runtime prefix remains 425,506 bytes. Its
34.597685583-second result is instrumented timing, not release performance.

A separate 356-byte feature source produces 278 packed bytes and exercises the
lazy activity maps and scalar rows. It peaks at 690,392 owned bytes, allocates and
frees 1,079,392 bytes, and leaves zero live bytes. Compared with the intermediate
activity-only feature run, peak increases 27,928 bytes (+4.22%). Aggregate phase
313 reports two passes with observation rows, zero changed rows and one restored
row. The instrumented 2.042653542-second result is not release performance; the
old baseline rejected this feature, so no feature speedup comparison is claimed.
Instrumented image SHA-256:
`f648d3d690526866d86e032c9fc11290e6ef4116db13f9882a9e3452b58f0e5d`.
No physical A6000 speed, full current-source self-host or 2 MiB fit is claimed.

## Mapped-flat repair

The mapped-flat v3 release-control image is 678,120 bytes with 711,228 linked
reservation bytes, 260 bytes above activity v7. Map v2 and v3 artifacts are
byte-identical. All ten former mapped owner cases pass freshly and exactly on
6502 and 68020. Seven mapped Hunk cases match exactly, including reversed output
selection, dependency-first assembly and two logical destinations; the Hunk BSS
instruction-relocation case also matches. The focused repair covers actual-geometry
slot lookup, concrete first-declaration ordering, logical alias destinations,
contiguous-flat selection and separate BSS `initializedLimit`/`size`. It adds no
state memory growth.

The unchanged release control takes 33.330524583 s versus 33.291479875 s for
activity (+0.12%, one pair). Separate MEMD unchanged-control telemetry is
unchanged: 1,282,360-byte peak, 3,182,136 bytes allocated and freed, zero live
bytes, 27,022 packed bytes and 425,506-byte runtime prefix. The 34.441206458 s
instrumented run is not release timing. The existing 112-case activity matrix
was not rerun; these focused results do not qualify a fresh full self-host or
2 MiB fit. Logs: `/tmp/opforge-mapped-flat-v3-release-control.log`,
`/tmp/opforge-mapped-flat-v3-memory-control.log`,
`/tmp/opforge-mapped-flat-v3-ds-native.log`,
`/tmp/opforge-mapped-flat-v3-hunk-bss.log`,
`/tmp/opforge-mapped-flat-v3-hunk-mapped-native.log`; prior ten-case proof:
`/tmp/opforge-mapped-flat-v2-mapped-native.log`.

## Control labels and forward member preparation

The unchanged 11,592-byte imported-macro control was run serially on
FS-UAE `68020-10m`, with no concurrent builds and telemetry disabled. Each state
used a frozen executable and matched the same live Rust output exactly. Source
SHA-256: `389d599e990455313064e23ab3d7cbf57ced5ecf45601f04421533eb393f12bd`.

| State | Release seconds | Change from preceding state | Image bytes | Linked bytes |
|---|---:|---:|---:|---:|
| Mapped-flat baseline, fresh repeat | 33.154801958 | — | 678,120 | 711,228 |
| Control labels and output policy | 33.395505458 | +0.73% | 678,624 | 711,740 |
| Forward member acquisition/count publication | 33.462413125 | +0.20% | 678,952 | 712,060 |

These are single samples, not statistically established slowdowns. The repeated
baseline differs from its earlier 33.330524583 s result, illustrating run variation.
Label publication uses shared packed PRVM classification and enclosing activity;
section geometry alone no longer enables Hunk relocation restrictions. Forward
acquisition creates numeric member plans after declarations and refreshes the
exported identity count. It adds no text fallback or typed observation cache.

Separate instrumented controls both peak at 1,282,376 owned bytes: +16 bytes for
labels/output policy versus the earlier mapped baseline, then no additional peak
growth for forward acquisition. Both allocate/free 3,182,152 bytes and finish at
zero live bytes. Packed source remains 27,022 bytes and runtime prefix 425,506
bytes. Their 34.670518959 s and 34.769237875 s times include instrumentation and
are not release measurements. Linked reservation is separate from owned heap;
these controls do not qualify the 2 MiB budget or a new full current-source
self-host.

The combined image passes 24 labeled-control/address cases (22 exact artifacts, two
expected diagnostic rejections) and 16 forward/member cases (ten exact artifacts,
six expected rejections) across 6502 and 68020. The prior frozen output-policy
image separately passes eight Hunk cases (two exact long-relocation artifacts,
six expected rejections). Rejection results do not establish exact diagnostic
text parity. Two additional placed-word controls were qualified in a separate fresh batch;
the main label matrix contains 22 cases. Full opcore coverage remains pending.

Frozen inputs, images, logs and the five-run measurement summary are retained at
`/tmp/opforge-shared-bindings-combined-20261011`; the output-policy image and Hunk
log are at `/tmp/opforge-labeled-controls-20261011`. The measurement helper runs
release states first and telemetry states separately; all five runs require fresh
completion and exact Rust output before recording a time.

## Match controls and packed expression framing

The unchanged imported-macro control above was run serially on FS-UAE
`68020-10m`, with telemetry disabled and builds held. Every frozen state
completed freshly and matched its live Rust artifact exactly. The source and
settings are unchanged; the results isolate each measured change.

| State | Release seconds | Change from preceding state | Image bytes | Linked bytes |
|---|---:|---:|---:|---:|
| Current forward/member baseline | 33.496751000 | — | 678,952 | 712,060 |
| Initial scalar-capsule extent repair | 33.321528125 | -0.52% | 678,984 | 712,092 |
| Sorted disjoint output spans | 33.527695042 | +0.62% | 679,148 | 712,252 |
| Central opaque capsule framing | 33.563650125 | +0.11% | 679,340 | 712,396 |
| Shared match controls | 33.742193500 | +0.53% | 680,696 | 714,308 |

These are single samples; differences under 0.7% do not establish a speed
change. They measure unchanged-path cost, not the speed of new match syntax.
The match image predates the configuration classification-collision repair.
The table also excludes later grouped-tuple transport and inserted-span
coalescing changes. Its exact results remain valid for this control, but do not
qualify those subsequent changes or complete opcore parity. Heap impact requires
separate instrumented runs. These measurements establish neither a new self-host nor 2 MiB fit;
[current self-host proof](../plans/compact-frontend-vm-boundary.md#full-current-source-self-host-qualification) is separate.

The frozen images, hashes and linked reservations are recorded in
`/tmp/opforge-opcore-controls-20261011.json`; per-state logs are
`/tmp/opforge-opcore-<state>-20261011.log`. The run helper is
`/tmp/opforge-measure-opcore-controls-20261011.py`.

### Separate follow-up costs

On 2026-10-12, the same 11,592-byte control (1,200 output bytes) was run
serially with the same `68020-10m` configuration. Builds were held throughout.
All ten runs completed freshly with exact live Rust output. Seven release
images were timed without telemetry; three separately instrumented images
provided allocation measurements. Compare the identified pair for each change,
rather than attributing a combined delta to the latest edit:

| Change | Before seconds | After seconds | Separate delta | Image growth | Linked growth |
|---|---:|---:|---:|---:|---:|
| Coalesce inserted output spans | 33.650178833 | 33.408906250 | -0.72% | 64 bytes | 64 bytes |
| Group opaque expression capsules | 33.721100791 | 33.353201833 | -1.09% | 180 bytes | 168 bytes |
| Classify outer operand tuples | 33.811678917 | 33.567695750 | -0.72% | 32 bytes | 32 bytes |
| Static include graph acquisition and CPU bootstrap | 33.567695750 | 34.875348041 | +3.90% | 1,520 bytes | 1,748 bytes |

Each timing is one sample. Small negative deltas do not establish speed gains;
these are unchanged-path cost checks, not timings of every new language form.
Static acquisition adds a modest measured cost while extending graph coverage.
The final release image is 682,492 bytes with 716,320 linked reserved bytes.

| Instrumented state | Peak owned bytes | Allocated/freed bytes | Live bytes |
|---|---:|---:|---:|
| Match/configuration baseline | 1,282,824 | 3,182,600 | 0 |
| Outer tuple classification | 1,282,824 | 3,182,600 | 0 |
| Static acquisition | 1,282,840 | 3,182,616 | 0 |

Packed source (27,022 bytes) and runtime prefix (425,522 bytes) are unchanged.
The static acquisition heap delta is 16 bytes. Owned allocation accounting
excludes executable/static reservations and OS costs; this is not proof of 2 MiB
fit. Instrumented stage times locate the additional work mostly in binding/raw
records (12.78 to 13.60 seconds) and module discovery (0.91 to 1.09 seconds).
Those stage times include telemetry and must not replace release timings.

The maintained helper is `scripts/performance/measure_compact_control.py`.
With the usual FS-UAE environment configured, supply frozen Hunk images and a
compiled test executable containing `native_active_include_control_fs_uae`:

```sh
python3 scripts/performance/measure_compact_control.py \
  --test-binary /tmp/current-asm-tests \
  --release-image before=/tmp/before-cli \
  --release-image after=/tmp/after-cli \
  --memory-image after-memory=/tmp/after-memory-cli \
  --report /tmp/compact-control.json
```

It records image/test hashes, configuration, completed exact timings, decoded
memory receipts and per-state logs. It refuses existing report/log output and
stops on failure while retaining the partial report. The present identified
images and receipts are in `/tmp/opforge-separate-controls-20261012.json`, with
logs in `/tmp/opforge-separate-controls-20261012-logs`. These controls do not
establish full opcore parity or a new self-host result.
The helper executes images serially and passes `--test-threads=1` to the saved
test executable. The `cargo test --no-run` command above only builds that
executable; it launches no emulator. Keep the normal runner's single-instance
configuration for each real-native measurement.

### Shared statement preparation foundation (BS38)

The 2026-10-12 foundation adds shared PRVM signature/head/matcher services,
validated preparation programs and numeric template interfaces. The unchanged
11,592-byte two-module control expands 1,200 macro invocations into 1,200 bytes;
it does not exercise statement invocation. CLI integration followed separately.
Both release runs have fresh exact output and zero guest exit on FS-UAE
68020/10 MiB, with instrumentation disabled.

| Measure | Before (BS37) | After (BS38 foundation) | Separate delta |
|---|---:|---:|---:|
| Release START-to-DONE seconds | 34.697495584 | 34.898929042 | +0.201433458 (+0.58%) |
| Release Hunk bytes | 682,492 | 685,364 | +2,872 |
| Linked reserved bytes | 716,320 | 719,152 | +2,832 |
| Peak owned bytes, separate instrumented run | 1,282,840 | 1,282,872 | +32 |
| Total allocated / freed bytes | 3,182,616 / 3,182,616 | 3,182,648 / 3,182,648 | +32 / +32 |
| Live owned bytes after cleanup | 0 | 0 | 0 |
| Runtime prefix bytes | 425,522 | 425,546 | +24 |
| Packed source bytes | 27,022 | 27,022 | 0 |

These are single samples; the small timing change does not establish a stable
regression. Linked reservations and peak owned storage are different measures;
OS costs are excluded, so this is not 2 MiB qualification. Instrumented timings
(35.924272334 and 35.905929458 seconds) are not release performance results.
Image/test hashes, environment and decoded memory receipts are retained in
`/tmp/opforge-statement-foundation-costs-20261012.json`, with per-run logs beside
it. Statement-execution timing, complete opcore and current full self-host remain
pending.

### Connected statement preparation and invocation (BS38)

The following 2026-10-12 comparison isolates source/template integration from
the preceding shared-service checkpoint `151ba938`. Both native images use
identical current CPU packages, generated by the same Rust test implementation;
the baseline native files come from a separate Git source snapshot. The unchanged
two-module control above has fresh exact output, completed protocol and zero
guest exit in each run on FS-UAE 68020/10 MiB.

| Measure | Before integration | After integration | Separate delta |
|---|---:|---:|---:|
| Release START-to-DONE seconds | 34.904874125 | 36.037844917 | +1.132970792 (+3.25%) |
| Release Hunk bytes | 685,512 | 707,136 | +21,624 |
| Linked reserved bytes | 719,288 | 740,092 | +20,804 |
| Peak owned bytes, separate instrumented run | 1,282,872 | 1,285,752 | +2,880 (+0.22%) |
| Total allocated / freed bytes | 3,182,648 / 3,182,648 | 3,185,528 / 3,185,528 | +2,880 / +2,880 |
| Live owned bytes after cleanup | 0 | 0 | 0 |
| Runtime prefix bytes | 425,546 | 425,546 | 0 |
| Packed source bytes | 27,022 | 27,022 | 0 |

Release timing is one sample per image; the separate instrumented runs take
36.113362 and 37.391815 seconds and are not release performance results.
This control measures cost added to existing macro work, not statement throughput.
Ten focused connected statement cases and five positive module-graph cases
separately match live Rust output; three negative graph cases reject freshly.
Calls inside expanded bodies and complete opcore coverage remain pending; no current
full-self-host, hardware or 2 MiB claim follows from this comparison.

Receipts with image/test hashes and environment are retained outside the build
cache in `/tmp/opforge-statements-connected-cost-20261012.json` and
`/tmp/opforge-statements-connected-before-memory-20261012.json`; the helper keeps
per-image logs beside each report. Reproduce with
`native_iterable_loops_host_assembles`, `OPFORGE_ITERABLE_BUILD_ROOT` for the
baseline native snapshot, `OPFORGE_ITERABLE_SAVE_IMAGE` for both images, and
`scripts/performance/measure_compact_control.py` as described above. Use a separate
`OPFORGE_ITERABLE_MEMORY=1` build for memory collection.

### Expanded-body statement invocation (BS38)

This subsequent 2026-10-12 slice adds optional owned body recipes and explicit
lexical-freshness/inherited-label metadata. The template owner keeps packed
execution when no statement definitions need those facets. Ten focused native
expanded-body cases match live Rust, and a plain macro control proves that packed
fallback. The unchanged 11,592-byte release control is measured separately against
the connected-call checkpoint `818ca2af`, on FS-UAE 68020/10 MiB.

| Measure | Before expanded-body support | After | Separate delta |
|---|---:|---:|---:|
| Release START-to-DONE seconds | 36.030338375 | 36.227721417 | +0.197383042 (+0.55%) |
| Release Hunk bytes | 707,136 | 707,856 | +720 |
| Linked reserved bytes | 740,092 | 740,812 | +720 |
| Peak owned bytes, separate instrumented run | 1,285,752 | 1,285,768 | +16 |
| Total allocated / freed bytes | 3,185,528 / 3,185,528 | 3,185,544 / 3,185,544 | +16 / +16 |
| Live owned bytes after cleanup | 0 | 0 | 0 |
| Runtime prefix / packed source bytes | 425,546 / 27,022 | 425,546 / 27,022 | 0 / 0 |

These are single release samples; the instrumented after run takes 37.500862458
seconds and supplies memory evidence only. The baseline memory row comes from
the preceding integration measurement. This small macro control does not measure
optional-recipe storage for a large project with many template bodies, statement
throughput, full self-host or hardware/2 MiB qualification. Receipts and per-image
logs are outside the build cache at
`/tmp/opforge-statements-expanded-cost-20261012.json`; reproduce with the same
image-build and control-measurement commands above.

The subsequent empty-token body guard is measured separately against `a7b15d6e`
on that same unchanged release control: 36.251036417 to 36.245907375 seconds
(-0.005129042 seconds, effectively unchanged single samples). Hunk size is
707,856 to 707,864 bytes; linked reservation is 740,812 to 740,820 bytes.
The guard adds no allocations or layout fields; heap use was not remeasured.
Fresh native comparison covers the original `statement_boundary_span` example
and a focused blank/comment-only body, including a comment without whitespace
after its semicolon. Receipt:
`/tmp/opforge-statement-empty-cost-20261012.json`.

### Numeric flat origins and output image

This subsequent slice permits forward/backward `.org` changes and normalizes
later initialized writes over earlier spans. The raw Bin writer streams that
same numeric image through the reused 4 KiB transport buffer, filling origin
holes without allocating their address extent. Placed-section/Hunk geometry
keeps its existing owner; existing reservation materialization is unchanged.
Sixty fresh connected cases match live Rust: ten origin scenarios, each on
6502 and 68020, with Bin/Hex/S-record artifacts. Nineteen separate native span
controls compare interval overlays with an independent byte-address model.

The unchanged 11,592-byte control is measured separately against `3614955e`,
with identically regenerated package inputs and FS-UAE 68020/10 MiB:

| Measure | Before origin support | After | Separate delta |
|---|---:|---:|---:|
| Release START-to-DONE seconds | 36.213061584 | 36.237453667 | +0.024392083 (+0.067%) |
| Release Hunk bytes | 707,864 | 709,392 | +1,528 |
| Linked reserved bytes | 740,820 | 742,324 | +1,504 |
| Peak owned bytes, separate instrumented runs | 1,285,768 | 1,285,768 | 0 |
| Total allocated / freed bytes | 3,185,544 / 3,185,544 | 3,189,896 / 3,189,896 | +4,352 / +4,352 |
| Live owned bytes after cleanup | 0 | 0 | 0 |

Single release samples establish no measurable large regression, not a speed
improvement. The separate instrumented runs take 37.420851 and 37.518597917
seconds and supply memory evidence only. This control does not quantify a large
overwrite-heavy image, full self-host, hardware or the 2 MiB target. Reproduce
with the preceding image-build/control commands, selecting this checkpoint and
its predecessor; receipt and per-image logs are outside the build cache at
`/tmp/opforge-origins-cost-20261012.json`.

The separate Motorola package repair (`abd177df`) regenerates the embedded
selectors on the same native origin/output implementation. Its unchanged control
measures 36.395603667 to 36.254972459 release seconds (single samples), with
Hunk and linked reservation both increasing 7,080 bytes. Separate instrumented
runs retain the same 1,285,768-byte peak, 3,189,896-byte total allocation/free
and zero live ownership. This cost belongs to the package repair, not the origin
slice. Receipt: `/tmp/opforge-motorola-package-cost-20261012.json`.

### Dotted binary output metadata

Shared PRVM now decodes BIN ranges and FILL bytes; separate preparation, path
policy and streaming owners consume numeric results. Thirty-two fresh positive
cases compare every requested artifact and inventory with live Rust; fifteen
negative cases complete with fresh rejections (not exact diagnostic-text parity).
The renderer has 29 independent native controls. Four instrumented metadata cases
verify owned spec vectors are released. Metadata blocks, target overrides, listings,
CLI range/fill options and section-metadata geometry remain unqualified.

On the unchanged 11,592-byte 68020/10 MiB control, initial metadata integration
measures 36.296708584 to 36.408205709 release seconds (+0.31%, single samples).
Image grows 716,472 to 718,144 bytes (+1,672); linked reservation grows 749,404
to 751,520 (+2,116). Separate instrumentation retains a 1,285,768-byte peak,
3,189,896 total allocated/freed bytes and zero live ownership. Receipt:
`/tmp/opforge-metadata-bin-cost-20261012.json`.

Final BIN-vector filename preflight is measured independently: 36.263818042 to
36.482956750 release seconds (+0.60%, single samples), on the same control.
It adds 52 image bytes and 40 linked bytes; no additional allocation is introduced.
These small timing differences do not establish a significant regression or gain.
Receipt: `/tmp/opforge-metadata-preflight-cost-20261012.json`. Full current-source
self-host time and the product memory floor require separate qualification.

### Package member preparation and current full self-host

The 2026-10-13 package-member repair has its own unchanged 11,592-byte
`68020-10m` control: 41.933110667 seconds release before, 41.867839750 after
(-0.16%, one pair; no measurable speed change). Executable size grows 660 bytes
and linked reservation 652 bytes. Separate instrumented peak grows 16 bytes,
1,240,696 to 1,240,712; allocated/freed totals both grow 16 bytes and live
ownership remains zero. Receipt: `/tmp/opforge-member-syntax-cost-20261013.json`.

The earlier unembedded self-host of `eed674a3` completes with exact live Rust
251,704-byte Hunk equality on FS-UAE 68020/74 MiB. It covers 193 files and
2,122,797 source bytes, with phase/memory instrumentation enabled. Fresh
START-to-DONE takes 3,568.574592333 seconds (59m28.57s). Guest measurements:

| Responsibility | Instrumented seconds |
|---|---:|
| Preparation overall | 2,329.72 |
| Binding and raw records | 1,158.65 |
| Module discovery | 371.93 |
| Runtime finalization | 224.83 |
| Expression preparation | 219.10 |
| Tokenization | 210.33 |
| Source I/O and other | 144.60 |
| Package setup | 0.28 |
| Assembly overall | 1,241.58 |

Substages are within preparation; do not add them to the overall preparation row.
Guest phase/EClock figures and host START/DONE observations are distinct clocks.
Tracked peak is 47,319,416 bytes. All 108,612,240 allocated bytes are freed, with
zero live ownership and no allocation failures. Source manifest is
`fnv1a64:befc1361cc9722cd`. The earlier incomplete runs and BS35 release run use
different inputs or measurement modes; these full-run times do not isolate this
repair or establish release speed, hardware timing or 2 MiB feasibility.
Receipt/output: `/tmp/opforge-selfhost-member-capture-20261013/`.

The subsequent complete embedded release self-build of `7a903cf8` matches the
entire live Rust 706,888-byte Hunk on FS-UAE 68020/74 MiB, with fresh exit zero.
All 194 mapped inputs total 2,580,296 bytes including the unchanged 454,890-byte
m68020 package. Bootstrap and output both embed that package. Release START/DONE
is 3,512.267856959 seconds (58m32.27s), with instrumentation disabled. This is
not a timing comparison against the earlier unembedded/instrumented case, and
does not establish hardware timing or a new full-workload memory measurement.
Capture: `/tmp/opforge-binary-assets-full-capture-20261013/`.

The same release bundle passes a fresh physical A6000 run in 118 guest seconds
(one-second resolution); host command time is 118.3560407909099 seconds including
connection. Instrumentation defines are empty. All 194 mapped inputs and the
complete 706,888-byte output identify the same case; its hardware output SHA-256
matches the complete emulator and Rust outputs. Receipt:
`/var/folders/98/663my3_95hqdv6wyjc99vkrw0000gn/T/opforge-a6000-result-ewffglzb/result.json`.
This is the physical release baseline for the [current resource diagnosis](../plans/native-performance-diagnosis.md).

The same full workload's separate instrumented A6000 run succeeds with exact
complete output in 191 seconds. Preparation/assembly phase clocks are 155.38 /
35.14 seconds. Tracked peak is 46,880,904 bytes, versus 2,895,104 live at assembly
entry; all 109,772,320 allocated bytes are freed with no failure or profiling
errors. The approximately 62% instrumented elapsed overhead must not be treated
as a release regression or applied uniformly to stage times. Binding/raw records
account for 100.79 instrumented seconds, about 65% of preparation. Full stage,
ownership and work-count analysis is maintained in the resource diagnosis above.
Receipt: `/var/folders/98/663my3_95hqdv6wyjc99vkrw0000gn/T/opforge-a6000-result-4i2o6ro3/result.json`.

The focused capture observer's current 195-input full A6000 run completes in
134 instrumented seconds with fresh exit zero and exact 706,888-byte Rust output.
It omits nested timing probes; the difference from 191 seconds is measurement
overhead/mode, not an optimization. Capture usage/reservation is 23,138,704 /
31,944,704 bytes; 8,806,000 bytes are slack. Whole tracked peak is 47,077,256 bytes.
Explicit record clearing is 23,195,144 bytes; disjoint accounted copy categories
total 100,452,613 bytes. All ownership is released and telemetry errors are zero.
The [resource diagnosis](../plans/native-performance-diagnosis.md#enduring-earlier-decisions)
selected removal of overwritten record clearing first, with separate fixed-input
release timing and no expected heap reduction.
Receipt: `/var/folders/98/663my3_95hqdv6wyjc99vkrw0000gn/T/opforge-a6000-result-5c03wy1h/result.json`.

The clear-elision implementation at `120e495b` clears only the reserved word
and both alignment gaps. The unchanged 11,592-byte release control takes
41.808061396 seconds before and 41.444949854 after (two samples each), a 0.87%
mean reduction on FS-UAE 68020/10 MiB. The separate full A6000 release comparison
uses identical frozen source/package/command/oracle and takes **117 seconds
both before and after**: no measurable gain at one-second guest resolution.
Host command intervals include connection overhead; their 0.4733-second
difference is not an isolated assembly-speed measurement. Current-source full
self-host also completes in 117 seconds, with fresh exit zero and exact complete
706,916-byte Rust output. The executable and linked reservation grow 28 bytes;
allocation policy/record extents are unchanged and no heap saving is claimed.
Full-workload instrumented heap/work for this candidate remain unmeasured.
The [comparison record](../plans/native-performance-diagnosis.md#enduring-earlier-decisions)
summarizes the retained timing conclusion; the receipts above identify the runs.

The opaque-asset repair's separate no-asset 11,592-byte control remains unchanged:
41.802194583 seconds before and 41.802430625 after, release mode on 68020/10 MiB.
Image and linked reservation grow 292 and 268 bytes respectively. Separate
instrumented controls retain the same 1,240,712-byte tracked peak and free all
3,145,864 allocated bytes. This measures overhead outside the asset path, not
asset processing gains. Receipt: `/tmp/opforge-binary-assets-cost-20261013.json`.

The separate asset-path comparison uses identical 454,890-byte printable ASCII
input, the same package, complete source and live Rust Hunk oracle, with release
images on FS-UAE 68020/10 MiB. Both revisions complete with fresh exit zero and
exact 454,948-byte Hunk equality. Reverse-order samples are:

| Image | START-to-DONE samples (seconds) | Mean |
|---|---|---:|
| Before opaque bytes | 16.974033250, 17.255742500 | 17.114887875 |
| After opaque bytes | 12.413317375, 12.182063958 | 12.297690667 |

These samples show 28.15% less elapsed time (1.39x throughput) for this valid
asset workload. They do not predict full self-host speed or cover arbitrary
binary input on the old executable, which rejects the actual package asset.
Asset SHA-256: `6a25b2a260d42e5437ae041b30d749f3a661c913a87b0f6470d28e05f6759960`.
Receipt with image/package/source/oracle identities:
`/tmp/opforge-ascii-asset-cost-20261013.json`. Reproduce with the ignored
`ascii_asset_timing_fs_uae` test and `OPFORGE_ASSET_TIMING_IMAGE` set to each
release executable, using the same configured FS-UAE environment.

Earlier approximately 40-minute self-host captures ended in exit 20 (for example,
2,380.911178041 seconds in `/tmp/opforge-selfhost-index-full-20261013.log`). Their
time to failure is not the duration of a complete assembly and cannot establish
a 40-to-60-minute full-build regression.
