# opForge Rust VM and Native AmigaOS Performance Baseline v0.1

## Purpose and provenance

This document records the verified architecture, existing measurements, static
costs, measurement gaps, and candidate bottlenecks that underpin the companion
Rust-first optimization plan. It is an investigation artifact, not a claim that
any optimization has been implemented.

- Inspected repository: `/Users/erik/Code/Retro/opForge-wt-rust-vm-native-performance`
- Branch: `codex/rust-vm-native-performance`
- Remote activation base: `68cc693c40fd27e30bed11e08974d3263d6cb6f6`
- Inspected planning-merge HEAD: `940a9e0d` (before the Item 1 activation commit)
- Base status: clean
- Host: macOS 26.6.2 (25G83), Darwin arm64, Apple T6041
- Rust: `rustc 1.95.0 (59807616e 2026-04-14)`, Cargo 1.95.0
- Available host sampler: `/usr/bin/xctrace`
- Not found on `PATH`: `cargo-flamegraph`, `samply`
- Native runner found: `/Applications/FS-UAE.app/Contents/MacOS/fs-uae`
- Investigation date: 2026-09-01

The task source exists only as an uncommitted file in the invoking checkout at
`dev-docs/NextSteps/opforge-native-performance-plan-codex-prompt-v4-rust-first-separate-worktree.md`.
It was treated as read-only external task input and was not copied into this
worktree. No current performance timings were collected during this planning
task; every number below is either an exact static calculation or historical
repository evidence and is labelled accordingly.

## Activation checkpoint and related work

The performance program is active from fetched remote checkpoint `68cc693c`.
That commit intentionally parks Item 40 and Milestone 8 of
`documentation/plans/opforge-native-amigaos-680x0-full-support-self-hosting-plan-v0_1.md`
until performance work makes the terminal generation-zero to generation-one to
generation-two proof practicable. The user's 2026-09-01 activation direction
accepts the parking checkpoint as the integration base; it does not claim that
either open checkbox passed.

The checkpoint records a canonical Rust package-only directory build completing
in 29.14 seconds with a 554,144-byte Hunk, 11-byte S-record, and 10,096,156-byte
listing. The unchanged native generation-one build remained CPU-active in the
reviewed A4000/68040 maximum-speed JIT profile for two hours and again for six
hours without guest `DONE`, guest exit, stdout/stderr diagnostic, or output
artifact. Both runs failed closed; generation two never started. A later
24-hour-ceiling attempt was stopped at the user's direction and is not evidence.

This makes bounded observation Items 0a-0e the first implementation sequence
after activation, followed by Item 2 corpus freeze and Item 0f attribution.
They may expose progress, counters, coarse timing, sampling symbols, and
explicitly incomplete abort snapshots, but may not optimize production behavior,
weaken the terminal proof, or represent an incomplete run as success.

### Field observation motivating the instrumentation bridge

On 2026-09-01 the maintainer reported that a native self-assembly on an A6000
had run for more than 4 hours 45 minutes. Drive activity was intense for the
first few minutes and then became silent for hours while the machine continued
working. The machine reports roughly 120 MIPS with its 68020-class execution
environment. This is a field observation, not instrumented attribution: the
MIPS value is not a literal instruction counter and the run had not necessarily
finished.

The scale is nevertheless a useful sanity check. Four hours 45 minutes is
17,100 seconds; at 120 million instruction-equivalents per second that is about
2.052 trillion instruction-equivalents. Across roughly 50,000 source statements
that is about 41.0 million per source statement. Even assuming one pass-one
visit plus all eight permitted pass-two visits (about 450,000 statement-pass
visits), it is about 4.56 million instruction-equivalents per visit. An
order-of-magnitude error in these assumptions would not make that normal
assembler throughput. The observation raises the priority of finding repeated
work, an algorithmic explosion, or a local spin before selecting any fix.

The early drive activity means bytewise native I/O may still impose a sizable
startup cost, but it is unlikely to explain the hours-long steady-state phase by
itself. F1 remains measurable work; it is no longer a defensible first
explanation for the complete elapsed time without operation counts.

The archived
`documentation/plans/completed/opforge-vm-runtime-performance-refactor-plan-v0_1.md`
is the source of the historical Rust measurements below. This program
complements its completed preparation and frontend optimizations and absorbs its
still-unchecked tooling-trace/feature-stripping question into evidence-led Phase
1 profiling. It does not silently reopen or supersede completed work.

The opFoundryCore design
`docs/planning/11_Amiga_Remote_Test_Execution_Architecture_Design.md` defines a
future neutral suite/job protocol, OFTB bundle, OFTR result, durable spool, fresh
challenge, exact exit/artifact proof, and equivalent FS-UAE/physical-Amiga
execution. opForge continues to own benchmark corpora and semantic oracles.
Remote execution can later automate native deployment and result collection,
but is not a prerequisite for Rust profiling or early native work-elimination.

## End-to-end execution pipelines

### Rust CLI, assembler, and package runtime

1. `crates/opforge-cli/src/bin/opforge.rs` enters the CLI and delegates to
   `opforge-cli-core`.
2. `crates/opforge-cli-core/src/run.rs::run_with_cli_with_context` validates the
   request. `run_one` constructs `api::asm::Assembler` with input, output,
   package, CPU, and diagnostic options.
3. `crates/opforge-engine/src/source_graph.rs::load_module_graph*` loads the root
   and dependency graph. `crates/opforge-engine/src/lib.rs` coordinates the
   session and output workflow.
4. `crates/opforge-asm/src/engine.rs::Assembler` prepares source, executes the
   initial layout pass, runs bounded layout-stabilization retries when required,
   then executes final emission/output processing.
5. `crates/opforge-asm/src/runtime_model.rs` and
   `crates/opforge-vm/src/runtime_model_core.rs::RuntimeModelCore` select and own
   package-driven tokenizer, parser, expression, selector, encoding, fixup,
   branch, value, operand, and state programs.
6. `crates/opforge-asm/src/phase_profile.rs` can attribute broad phases and named
   execution paths. Artifacts are then rendered and written by the assembler and
   engine output paths.

The Rust stabilization design already separates layout retries from the final
artifact path more cleanly than the current native loop. It also reuses prepared
source and route caches introduced by the earlier performance plan. Those are
reference concepts, not byte-for-byte native representation requirements.

### Native AmigaOS/680x0 CLI and assembler

1. `native/motorola68000/amigaos/opforge-cli/run.asm` performs startup and an
   existence-only root-source open/close, then coordinates bootstrap discovery,
   package setup, assembly, and artifact output.
2. `native/motorola68000/amigaos/opforge-cli/source_reader.asm` supplies root
   source reading. `.output` and `.cpu` bootstrap paths, normal tokenization, and
   known-range skipping currently consume one byte per DOS `Read` call.
3. `native/motorola68000/amigaos/opforge-cli/module_discovery.asm` discovers and
   indexes module declarations once per invocation, but scans candidate files
   using one-byte reads.
4. `native/motorola68000/amigaos/opforge-cli/package_pipeline.asm` bulk-reads an
   external package. The embedded package is copied from `incbin` storage into
   the mutable package arena before the package service validates/activates it.
5. `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm` and service-specific
   files expose package/runtime requests. `opasm_tkpkg_bridge.asm` connects them
   to the assembler engine.
6. TKVM and PRVM tokenize and parse source into the fixed statement arena.
   `native/motorola68000/amigaos/opasm/opasm_engine.asm` owns statements,
   symbols, pass orchestration, image buffers, and callbacks to expression,
   selection, encoding, branch, fixup, operand, and state services.
7. Pass one runs once. Pass two runs up to eight layout-only retries with output
   disabled, followed by one explicit final-emission pass after convergence.
   This checkpoint already separates final materialization, but every pass-two
   boundary still clears the full 1 MiB image-presence map and revisits label,
   flow, selection, encoding, and PC work. Output is written through bulk
   artifact paths after a successful assembly.

Existing choices to preserve are the once-per-invocation module index, bulk
external-package reads, in-memory/bulk artifact writes, TKVM jump-table dispatch
and fused scanner operations, safe reuse of known statement sizes, and
package-owned target semantics behind CPU-neutral VM/service boundaries.

## Rust VM and native counterpart inventory

| Stable VM class | Rust primary entrypoint and owner | Native counterpart | Profiling focus |
|---|---|---|---|
| TKVM tokenizer | `runtime_model_core.rs` tokenizer execution; `execution_model/tokenizer_bridge.rs` | `tkvm/tkvm_runtime.asm`; `tkpkg/tkpkg_tokenizer_vm.asm` | program/opcode/PC, scanners, tokens, scratch, branches |
| PRVM v2 parser | `execution_model/parser_vm_v2.rs::parse_line_with_parser_vm_v2`; `vm_opasm.rs` | `prvm/prvm_runtime.asm`; `prvm/prvm_bridge.asm` | dispatch, checkpoints, token/lexeme work, expression resumes |
| EXVM parser | `exvm_v2_runtime.rs::run_exvm_expression_parser_program*` and `execute_from`; `vm_opcore.rs` | `opcore/opcore_expr_bridge.asm` frontend | parser bytecode, helpers, allocations, emitted ExprVM program |
| ExprVM evaluator | `crates/opforge-core/src/expr_vm.rs` portable compiler/evaluator; `vm_opcore.rs::evaluate_*` | `exprvm/exprvm_runtime.asm`; `opcore/opcore_expr_bridge.asm` | compile/bind/evaluate counts, symbols, stack, current-PC use |
| MSEL/TABL selection | `selector_vm.rs::execute_selector_program`; `runtime_model_core.rs::resolve_selector_choice` | `tkpkg/tkpkg_selection_service.asm` | candidate scans, predicates, selector/table identity |
| SEMV semantic bytecode | `bytecode.rs::execute_program`; `RuntimeModelCore::execute_semantic_program` | semantic/encode service paths in `tkpkg` | opcode/PC, operand reads, helper boundaries |
| Encoding VM | `encoding_vm.rs::execute_encoding_program` | `tkpkg/tkpkg_encode_service.asm` | selection-to-encoding transitions, output sizes |
| Structured encoding | `structured_encoding_vm.rs::execute_structured_encoding_program` | `tkpkg/tkpkg_encode_service.asm` | record decode, candidates, result high-water |
| OPRD operand records | `operand_record_vm.rs::execute_operand_record_program_with_records` | `tkpkg/tkpkg_operand_record_service.asm`; `tkpkg_operand_runtime.asm` | record decode, candidate/result counts |
| STVM state | `state_vm.rs::{initial_state,apply_directive,capability_allowed}` | `tkpkg/tkpkg_state_service.asm` | decode/reset/directive lookup and invalidation |
| BRVM branch | `branch_vm.rs::execute_branch_program` | branch work in `tkpkg_encode_service.asm`/runtime context | form choice, stability, branch outcomes |
| FXVM fixup | `fixup_vm.rs::execute_fixup_program_for_version` | fixup work in `tkpkg_encode_service.asm`/opasm bridge | fixup kind, helper calls, result/error paths |
| VALU value | `value_vm.rs::execute_value_program` | value/encoding service path | invocations, opcodes, stack/result sizes |
| CALS CPU aliases | decoded compact package data, not a standalone opcode interpreter | package selection/alias service | decode and lookup service cost; do not mislabel as VM |

This inventory is the minimum profiling scope. The implementation must discover
new package executors through a central registry/test so profile coverage cannot
silently drift when another VM is added.

## Existing measurement and profiling facilities

`crates/opforge-asm/src/phase_profile.rs` contains 41 phase buckets and two
environment gates: `OPFORGE_PROFILE_PHASES` and
`OPFORGE_PROFILE_EXECUTION_PATHS`. It uses thread-local accumulated durations
and counts and emits sorted summaries at a controlled boundary. This is useful
for coarse attribution and avoids per-event output, but path labels are dynamic
strings and output is human-readable stderr. It has no stable shared numeric ID
registry, VM/program/opcode/bytecode-PC attribution, transition histogram,
branch/high-water/allocation/clone/cache/accelerator counters, sampled timing,
bounded trace, machine-readable export, or overhead calibration.

No Criterion configuration, Rust `#[bench]` harness, `cargo-flamegraph`, or
Samply integration was found. The large-fixture regression test
`qualified_use_reachability_perf_regression_multi_module_fixture` is a guard,
not a controlled benchmark harness. The repository therefore has useful phase
telemetry and production-path parity tests, but no current VM profiler or
reproducible benchmark ledger.

### Historical Rust measurements

These results came from the archived runtime performance plan and are not a
current baseline. They are retained because they establish prior mechanisms and
guard against repeating rejected work:

| Completed change | Historical observed result |
|---|---|
| Prepared route cache | `assembly_total` 3819.788 ms; 25,332 stabilization hits and 12,666 pass-two hits |
| No-listing/listing split | listing work about 842 ms before; listing-on 788.137 -> 37.532 ms; no-listing 0.499 ms |
| Stabilization reduction | 985.069 ms/two retries -> 483.887 ms/one retry; total 2621.766 ms |
| Token reuse | total 2621.766 -> 2453.537 ms; pass-one parse 873.641 -> 720.332 ms |
| Parser route cache | total 2447.769 -> 2380.827 ms; parse 714.062 -> 627.076 ms; routing 317.640 -> 228.459 ms |
| Token clone removal | total 2380.827 -> 2353.859 ms; parse 627.076 -> 615.984 ms |
| Tokenizer attribution | total 2353.859 -> 2352.044 ms; tokenizer 228.928 ms, portable runtime 157.763 ms |
| Prevalidation | total 2352.044 -> 2290.573 ms; parse 604.961 -> 555.134 ms; portable runtime 157.763 -> 112.278 ms |
| Exact default-tokenizer fast path | total 2290.573 -> 2206.855 ms; parse 555.134 -> 486.772 ms; tokenizer 183.067 -> 115.530 ms |
| Rejected parser fast path | targeted bucket 412.556 -> 403.555/405.227 ms, but total regressed 2206.855 -> 2252.978/2274.575 ms; prototype `b3207be4`, revert `c3e06ff9` |

The parser result is a direct warning: component speedup, synthetic coverage, or
plausible dispatch reduction is insufficient without repeated end-to-end proof.

## Verified static sizes and workload evidence

### Native session arena

`opasm/opasm_engine.asm` defines:

| Region | Exact bytes |
|---|---:|
| Source records/text | `(100000 * 10) + 4194304 = 5,194,304` |
| Statement records | `100000 * 308 = 30,800,000` |
| Labels plus 256 hash heads | `(16384 * 127) + (256 * 4) = 2,081,792` |
| Three 1,048,576-byte image arrays | `3,145,728` |
| Header and tail | `104` |
| Total | **41,221,928** |

`initSessionV1` clears that full span through the byte-at-a-time `clearBytes`
loop. The cost is unconditional and capacity-based rather than live-data-based.
The checkpoint grew label rows from 123 to 127 bytes and each image array from
65,535 bytes to 1 MiB to support the current self-host artifact, increasing the
mandatory session clear by 3,014,663 bytes from the planning baseline.
Layout reset separately clears 100,000 statement section-index/mapped entries.
The statement record stores 108 label bytes, 64 operand bytes, and 64 owner
bytes: 236 string bytes per row, or 23,600,000 bytes at capacity. The runtime
already has a source-slice fallback for long operands, proving that offset-based
representation is possible in at least one path.

Current code comments cite 89,933 source/listing rows and 48,950
nonblank/noncomment statement rows. The active self-hosting plan records 90,441
Rust-expanded source rows. These refer to different measurement points and must
not be merged; Phase 0 must regenerate both with explicit definitions.

### Package, labels, and artifacts

- Embedded all-family package size: **368,278 bytes**.
- Package capacity: 393,216 bytes; static headroom: 24,938 bytes.
- Full-product evidence: 369/512 imports, 6,330/8,192 exports,
  122,694/262,144 name bytes, 9,134/16,384 labels, longest fully-scoped label
  107 bytes.
- With 256 label buckets, full capacity would imply a theoretical average of 64
  labels per bucket. The observed 9,134-label workload averages about 35.68 and
  recorded a worst bucket of 49. Only the latter is observed evidence.
- Activation-checkpoint self-hosting oracle evidence: Hunk 554,144 bytes,
  S-record 11 bytes, listing 10,096,156 bytes; Rust package-only directory build
  29.14 seconds.

## Findings

Classification means: **M** measured in retained repository evidence, **S**
statically verified high-confidence mechanism, and **H** plausible hypothesis
requiring instrumentation. A finding may have both historical measurement and a
static current mechanism, but no historical number is treated as current.

### F1 — byte-scaled native source I/O (S)

`source_reader.asm` passes a length of one to the DOS read wrapper during output
bootstrap, CPU bootstrap, normal tokenization, and known module-range skipping.
`module_discovery.asm` does the same while scanning candidates. `run.asm` first
performs an existence-only open/close. There is no native DOS seek wrapper.

For a root whose `.output` is selected and whose CPU must be discovered, the
current structure can perform the existence check plus separate output, CPU, and
normal-processing opens, with three bytewise scans that may terminate early.
Exact dynamic opens/reads depend on directives and dependency graph and must be
counted. Line ending and bounded-range behavior are semantic constraints for a
shared buffered reader.

### F2 — full 41,221,928-byte session clear (S)

The exact capacity calculation and `initSessionV1` byte-clear loop are verified.
The primary opportunity is to remove initialization of unused capacity via
authoritative counts, full record initialization, generations/touched ranges,
and debug poisoning—not merely replace the loop with a faster memset.

### F3 — avoidable native copies and bytewise primitives (S/H)

`opforge-cli/copy.asm` supplies bytewise generic copy/clear loops.
`package_pipeline.asm` copies the 368,278-byte embedded package into package
storage, while external packages already use a bulk read plus a one-byte overflow
probe. Immutable embedded package validation/execution through an active base
pointer is a high-confidence opportunity subject to lifetime/alignment checks.
Other copy/clear call-site importance remains unmeasured. Existing bulk output
writes should be preserved.

### F4 — residual layout-round work after final-emission separation (S/H)

Checkpoint `68cc693c` partially resolves the original finding:
`opasmEngineRunTwoPassV1` now disables output during up to eight layout retries
and schedules one explicit final-emission pass after convergence. The roadmap
must not reimplement that split. Residual repeated work remains measurable:
every pass-two boundary clears the full 1 MiB image-presence map, refreshes label
finalization state, and revisits flow, selection, size, encoding, and PC paths.
Counters must prove whether layout retries materialize zero image bytes, whether
exactly one final emission occurs, and which residual operations dominate.

### F5 — native expression compile on each evaluation (S/H)

`opcore/opcore_expr_bridge.asm::runEvalProgram` calls `compileExpression`, resets
and rebuilds a private 128-byte program, terminates it, then calls ExprVM for each
evaluation. Compile-on-evaluation is static fact; repeated identical-expression
frequency across pass one, retries, and final emission is not measured. Prepared
state must include scope, stable symbol identity, current-PC use, forward/unstable
references, pipeline/CPU state, diagnostics, and expression-contract version.

### F6 — repeated native directive routing and flow scans (S/H)

`opasm_directive_router.asm` classifies directives through a sequential series of
`directiveTry*` calls. `opasm_flow_navigation.asm` scans forward for IF branches,
matching ENDIF, selected MATCH branches, and related boundaries. Pass callbacks
revisit this work. The repeated structure is verified; frequency and share need
counters before representation changes.

### F7 — fixed 308-byte native statement rows (S)

The arena reserves 30.8 MB regardless of live rows, dominated by 23.6 MB of
inline label/operand/owner strings at capacity. A hot/cold record plus source
slices, pools, and interned owner IDs could reduce both reset work and cache
traffic, but this is a high-risk representation migration and follows profiles,
prepared-state work, and dual-representation proof.

### F8 — native symbol indexing pressure (M/S/H)

Exact lookup uses a 256-bucket hash-head/next chain and full name comparison. The
full-product evidence recorded a worst chain of 49. Final-component resolution
in `opasmEngineResolveUniqueLabelFinalComponentV1` scans every label and performs
length/suffix work for each candidate. The expression bridge's `resolveLabelIndex`
also linearly scans its complete label snapshot for each symbol reference, while
scoped operand resolution can cascade through several lookup strategies. With
9,134 labels and 48,950 nonblank/noncomment statements, one complete label scan
per statement would inspect about 447 million candidates per pass, or about
4.02 billion across nine statement visits. That is a scale illustration, not a
dynamic call count. Exact-call, candidate, and compared-byte distributions are
absent, so stored hash/length metadata, more buckets, a secondary final-component
index, and prepared symbol IDs remain candidates only after counters.

### F9 — STVM is decoded/scanned during reset and directive application (S/H)

`tkpkg_state_service.asm::initializeActiveV1` chooses the state owner and calls
`resetActiveV1`. Reset reparses profile/key/default/override records on every
assembly pass or pipeline switch. `applyDirectiveV1` linearly walks serialized
directives and compares case-folded strings. Decode-once state and indexed
directives are plausible, but invocation/share and invalidation behavior need
Rust-first and native counters.

### F10 — native dispatch/ABI overhead (H)

TKVM already uses an opcode jump table and fused scan handlers and should not be
regressed. PRVM and ExprVM use compare/branch opcode dispatch, and statement
passes use indirect callbacks and broad register saves. Whether dispatch,
register-save, helper crossings, code size, or cache effects matter on 68020 is
unknown. This is late native-only tuning after work-elimination and correlation.

### F11 — insufficient Rust VM attribution (S)

The current profiler cannot answer which VM, program, opcode, bytecode PC,
sequence, helper, allocation, clone, lookup, or service boundary dominates.
Without that evidence, generic VM acceleration would repeat the risk demonstrated
by the rejected parser fast path. A shared, low-overhead profiler is therefore
the mandatory first implementation phase.

### F12 — unattributed multi-hour native compute phase (M/S/H)

The A6000 observation establishes a multi-hour, apparently compute-bound phase
after initial drive activity but does not identify its owner. Static inspection
shows several plausible multiplication sites: pass one plus as many as eight
pass-two rounds; full-table final-component and expression-label scans; scoped
and fallback lookup cascades; repeated expression parse/compile/bind/evaluate;
control-flow traversal; selection/encoding candidate work; and native VM/service
execution. None has a measured dynamic share. Native progress and operation
instrumentation is therefore the highest-priority evidence gap. Until it exists,
changing lookup, I/O, dispatch, representation, or pass behavior would be a guess.

## Rust-first profiler architecture

### Identity and schema

Create a CPU-neutral versioned profile schema and central registry. Every record
uses stable numeric IDs for VM class, program, opcode, helper class, owner,
pipeline, phase, and accelerator. Program identity combines package-format
version, canonical package digest/length, owner, VM class, program-table key,
program offset/length, and bytecode-contract version. Reports include the
resolvable ID catalog, so Rust and native can correlate identities without
assuming equal addresses or timing behavior. Unknown/new IDs fail visibly in
schema validation rather than being folded into an anonymous bucket.

The durable export is versioned JSON Lines: one metadata/header record, catalog
records, counter/timing/high-water/histogram records, and a terminal integrity
record. JSONL permits bounded streaming at the run boundary and diff-friendly
host tooling. Native may accumulate a compact fixed-width binary record buffer
using the same numeric IDs and export once; a host decoder must emit the same
logical JSONL schema. Human-readable ranked tables are derived output, never the
only evidence.

### Modes and collection

- **off/control:** compiled or gated so hot events are effectively zero-cost;
  establishes the unprofiled baseline.
- **counters:** deterministic saturating invocation, opcode, PC, branch, helper,
  lookup, allocation/clone, cache, invalidation, accelerator, and high-water
  counts. Suitable for deterministic CI budgets.
- **sampled:** coarse inclusive/exclusive timing with deterministic configurable
  sampling and external `xctrace` call-stack sampling. No timer call per opcode.
- **trace:** a workload/VM/program/phase-filtered bounded ring containing IDs,
  PC, branch/result class, and sequence context. Overflow is counted and explicit.

All modes accumulate in bounded memory and export only at a controlled assembly
boundary. There is no per-event console/file I/O. Thread and nested-runtime
attribution must preserve inclusive/exclusive accounting. Counters cover stack,
scratch, candidate, token, result, scan, symbol, resume, state, and temporary
allocation high-water marks plus expression parse/compile/bind/evaluate and
accelerator eligible/hit/miss/fallback/bypass/mismatch/dual-run events.

### Calibration and reports

For every corpus/configuration run an unprofiled control, counters-only build,
sampled build, and—only on targeted cases—bounded trace build. Record wall-time
overhead, output size, dropped/overflowed samples, timer resolution, and result
stability. A metric whose mode materially perturbs the ranking is investigation
evidence only. The Phase 1 report must separate dispatch/check costs from
semantic/helper/allocation/lookup/service costs, list opcode/PC and pair/triple
coverage, quantify candidate accelerator coverage, and identify integrated-workload
regressions as well as wins.

### Accelerator lifecycle

Every initial generic VM accelerator is implemented in Rust first with
disabled, generic-only, enabled, and bounded dual-execute-and-compare modes.
Eligibility is package capability plus validated program identity/signature—not
path, source text, benchmark name, generation number, or expected output. Reports
record eligibility, hits, misses, fallback, bypass, mismatch, setup cost, code
size, memory, isolated speed, and end-to-end speed. The portable interpreter
remains the semantic oracle and compatibility fallback. A decision record marks
each accepted result portable-to-native, native-redesign-required, Rust-only, or
rejected/reverted. Native transfer requires a separate positive decision.

### Profile-gated portability design space

The portable bytecode need not remain the representation directly dispatched
on every vintage target. It can remain the canonical semantic and distribution
format while a toolchain derives a faster target representation. Candidate
families, evaluated only when profiles show VM dispatch/decode or a stable hot
program is material, are:

- a verified, architecture-neutral decoded micro-op/execution-plan cache that
  removes repeated decode and validation while retaining interpreter handlers;
- generated portable superinstructions selected from measured opcode sequences;
- generated direct/indirect-threaded handler tapes whose target backend maps
  portable opcode IDs to target-local handler addresses;
- offline partial evaluation of immutable package programs into a stable
  architecture-neutral low-level IR, followed by per-target assembly emission;
- exact-program/signature-bound cross-assembly from canonical VM bytecode into
  target object code, with the original bytecode, validator, and interpreter as
  oracle and fallback; and
- source-generated per-target interpreters sharing one declarative opcode/
  semantic description, where full AOT code size or relocation cost is too high.

A two-level design—canonical bytecode to validated portable execution IR, then
execution IR to a target backend—best preserves portability if AOT is selected.
Package semantics stay in the canonical definition; backends own only lowering,
calling convention, relocation, register allocation, and target code layout.
Every derived artifact is keyed by format/version/program digest/capabilities,
has deterministic regeneration, rejects stale/unknown input, and falls back on
the generic interpreter. The decision must compare setup time, code size, RAM,
relocations, cache pressure, generated-code verification, workload coverage,
and end-to-end benefit. On memory-constrained targets, predecode, threading, or
selected superinstructions may dominate full cross-assembly despite lower peak
speed.

## Native profiler and correlation design

The native facility begins with a bounded, memory-resident progress block. It
contains a schema version and fresh run identity; complete/incomplete state;
phase; pass/layout round; current and last-completed statement; total statements
and statement visits; current source/module and VM/service/program identity;
flow and backward-redirect counts; last-progress tick; and coarse phase elapsed
ticks. A low-frequency heartbeat may expose the block through the approved
native debug/assert framework—by a statement-visit quantum such as 4096 or a
tens-of-seconds interval—but is default-off and must have measured overhead.
There is no per-operation console or file output.

Deterministic counters then measure the multipliers rather than presupposing the
culprit:

- statement visits by pass; layout rounds and `LayoutChanged` reasons; final
  convergence status and convergence/final image bytes;
- exact, scoped, imported, and final-component lookup calls; candidate labels,
  compared string bytes, chain/probe histograms and maxima;
- expression parse, compile, bind, and evaluate calls plus expression-snapshot
  label candidates;
- directive classifications, flow rows visited, forward redirects, and backward
  redirects;
- native VM/service invocations and total opcodes by VM/program/phase, plus
  selector and encoding candidates; and
- bytes/ranges cleared and copied; DOS opens, reads, bytes, seeks, writes and
  closes; files, source bytes, logical lines, module candidates/declarations;
  and used/peak source, statement, label, image, and scratch memory.

Timing remains coarse: startup/package/source ingestion, statement building,
pass one, each layout round, final emission, and artifacts. There is no timer
call per opcode or lookup. FS-UAE or another emulator may additionally sample
the program counter against a retained symbol/map file; physical hardware
counters confirm the emulator diagnosis rather than substituting anecdotes for
counters.

A graceful diagnostic abort must export the same envelope with
`complete=false`, current progress, counters, overflow state, and elapsed ticks.
That makes a bounded five-to-ten-minute investigation useful without claiming a
successful assembly. Such a snapshot is localization evidence only and can
never satisfy Level D or close the active self-hosting proof.

Counter slopes guide the next experiment: a fixed statement with rising local
counters indicates a spin; explosive candidate-label/string-byte counts point
to lookup; expression compilation tracking statement visits points to missing
prepared expression state; repeated full statement counts point to the pass
multiplier; quadratic flow rows point to navigation; dominant VM opcode totals
justify VM-level profiling; and repeated convergence image bytes point to
layout/emission work. Only this evidence may reorder or select optimizations.

Export happens once at a controlled boundary: either the explicit diagnostic
abort above or fresh guest completion with explicit guest exit. Only the latter
can be proof. FS-UAE is a confirmation gate, not the inner-loop debugger, and
every case must retain the repository's fail-closed Level D proof contract. Later OFTB/OFTR
integration may carry profile configuration and result files, but must preserve
fresh challenge, exact completion/exit, artifact checksum, attempt-all, and
ephemeral run-tree rules.

## Benchmark matrix and measurement protocol

| Case | Mechanism isolated | Required production path |
|---|---|---|
| B01 tiny 10-line source | fixed startup/session/package cost | real CLI, real package |
| B02 ~1 MiB comments/whitespace | source I/O, line handling, tokenizer | real CLI input |
| B03 many trivial statements | parse/store/pass/callback throughput | normal statement pipeline |
| B04 label/symbol heavy | hash probes, comparisons, scope lookup | package-driven symbols |
| B05 forward branches | layout rounds, branch/fixup stability | current stability fixtures plus scaled case |
| B06 expression heavy | EXVM compile/bind/eval and symbol dependencies | normal expression service |
| B07 nested IF/repetition/MATCH | directive routes and flow boundaries | normal control-flow semantics |
| B08 module/include tree | opens, reads, seeks, module index, owner scope | real dependency graph |
| B09 all-output workload | final emission, listing/map/metadata/Hunk/S-record/BIN/PRG | requested production outputs |
| B10 bounded integrated production workload | cross-mechanism coverage and end-to-end performance | real CLI/package path; sized in Item 2 for repeatable native profiling |

Prefer existing fixtures; generated inputs are allowed only to isolate a
mechanism and must still traverse the real CLI/engine/package runtime. Record
HEAD, package bytes and identity, OS/architecture/toolchain, release profile and
features, CPU/pipeline/dialect, exact command, corpus revision/digest, outputs,
profiler mode, and cold/warm state. Native records add FS-UAE version/config,
CPU/JIT, memory, device mapping, and profile/debug flags.

Use a warm-up policy established in Phase 0, then at least seven retained runs
per configuration unless variance analysis justifies a different count. Report
median, minimum/maximum, and p95 (or all samples when the set is too small), not
a single run. Randomize paired before/after order where host noise matters.
Compare identical artifacts, diagnostics, exit codes, package and configuration.
Report mechanism counters and B10 integrated-workload wall time together.
Cold-start and warm-cache results remain separate. B10 must be large and varied
enough to exercise modules, symbols, expressions, flow, layout, emission, and
requested artifacts, but bounded enough for repeated control/counters/sampled
runs on the A6000. Item 2 records its exact composition, digest, coverage, and
runtime envelope rather than deriving it from benchmark identity in production
code.

Authoritative current gates include `make quality-gate`, the focused crate tests,
`python3 scripts/workflow/run_native_porting_quality_gate.py --staged`, and the
native FS-UAE parity/self-hosting wrappers. The parked terminal proof remains
`external_fs_uae_native_opforge_two_generation_self_host_parity`; after it passes,
the generation-two-first 53-test bonus wrapper is
`scripts/workflow/run_native_generation_two_bonus_completion.sh`. Profiling does
not invoke either as an inner loop, and their fresh-challenge/explicit-exit/
artifact-equality contracts remain unchanged. The full self-host is not a
performance benchmark, profiler calibration case, hotspot-ranking input, or
optimization acceptance run; it is an explicit terminal correctness/scalability
gate only.

## Architectural constraints and non-goals

- Rust is the semantic reference, profiling authority, and first implementation
  site for generic VM optimization.
- 68020 is the native baseline. 68080/AMMX is an optional later accelerator, not
  an answer to avoidable work or a semantic fork.
- Generic Rust/native VM and service code remains CPU-neutral and package-driven;
  CPU/family/dialect semantics remain in package/family definitions.
- Hunk, S-record, BIN/PRG, listing, map, metadata, exported-section, fixup,
  diagnostic, exit-code, state, symbol, and layout parity are mandatory.
- No benchmark-only path, fixture identity branch, reduced-workload claim,
  hidden fallback, per-event I/O, or weakened Level D proof is acceptable.
- Future 8-bit native implementations must not be constrained by an Amiga-only
  semantic shortcut.
- This baseline/activation-document update does not itself change production
  code, tests, fixtures, reference outputs, opFoundryCore, or the parked
  self-hosting plan; later checked implementation items do change their named
  production/test scopes.

## Prioritized finding table

| Priority | ID | Classification | Expected impact | Risk | Dependency |
|---:|---|---|---|---|---|
| 1 | F12 | M/S/H | Attributes the observed multi-hour native compute phase | Low/Medium | approved progress/counter bridge |
| 2 | F11 | S | Enables all defensible generic VM decisions | Medium | Phase 0 Rust profiler foundation |
| 3 | F8 | M/S/H | Potentially extreme repeated symbol work | Medium | lookup-call/candidate/byte slopes; no fix before evidence |
| 4 | F5 | S/H | Potentially high repeated expression work | High | parse/compile/bind/evaluate slopes and Rust design |
| 5 | F4 | S/H | Potentially high residual layout-round work | Medium/High | pass, presence-clear, and image-byte counters |
| 6 | F1 | S | High startup I/O call reduction; unlikely full steady-state explanation | Medium | DOS counters and buffered-reader contract |
| 7 | F2 | S | Very high fixed-startup work removal | Medium | initialization-invariant proof |
| 8 | F3 | S/H | Medium startup and memory-bandwidth reduction | Low/Medium | copy-site counters; package-base audit |
| 9 | F6 | S/H | Potentially high on control-flow cases | Medium | route/flow counters and maps |
| 10 | F9 | S/H | Medium for pass/pipeline-heavy inputs | Medium | shared state identity/invalidation profile |
| 11 | F7 | S | High memory/cache/reset potential | High | F2, prepared state, access profile |
| 12 | F10 | H | Unknown; possibly small after work removal | High | Rust decisions, native correlation, 68020 model |

Only priorities 1 and 2 authorize implementation before the first profile
reports: they are measurement foundations. Priorities 3-12 are provisional
mechanism hypotheses, not an optimization order.

## Remaining Phase 0 evidence

- Current repeated Rust wall-time and VM-attributed baseline; the checkpoint's
  single 29.14-second Rust build is activation evidence, not a benchmark series.
- Exact full-product definitions/counts for source rows versus live statements.
- Dynamic native DOS operation counts and bytes by source/bootstrap/module path.
- Native phase/progress location and counter slopes across bounded B01-B10
  A6000 runs; whether representative workloads progress, repeat complete passes,
  or spin inside one statement/service.
- Dynamic native bytes cleared/copied, pass rounds, convergence emission bytes,
  and used/peak arena sizes.
- Rust program/opcode/PC/helper/allocation/clone/transition distributions.
- Expression identity/recompile rates, directive/flow revisit rates, STVM reset
  rates, and symbol probe/string-compare distributions.
- Current FS-UAE version/configuration reproducibility and physical-Amiga timing
  availability.
- Instrumentation overhead and timer/sampler resolution.
- A symbolized emulator PC sample and physical-A6000 confirmation for the
  highest-ranked native compute owner.

No optimization claim is valid until the relevant gap is closed with the
specified production-path measurement and parity evidence.
