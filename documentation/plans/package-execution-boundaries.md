# Package-controlled execution: Rust and native

Status: B1 complete and locally validated. Review the findings
before selecting B2. Companion to [the runtime reset](native-runtime-reset.md),
following W3's measurements. The active [AGENTS.md](../../AGENTS.md) remains binding.

## Outcome and scope

Establish which work is actually controlled by canonical packages, expose hidden
target-semantic callbacks, and remove one demonstrated dependency through a shared
operation we can test on Rust and compare with native. This precedes using Rust VM
counts to judge native feasibility. Preserve the reset's cross-target efficiency,
compact-code and Amiga resource goals; this is not a whole-runtime rewrite.

The requirement is package-owned target semantics, not that every operation must
be many tiny bytecodes. Generic descriptor interpreters and substantial shared
primitives are legitimate. Specializations need explicit selection conditions and
equivalence to the canonical operation, including errors and changing state.

## Initial high-level source scan

Both implementations are hybrid. Native has real bytecode interpreters, but does
not simply implement all Rust fallbacks as equivalent VM programs. These are
source observations on the inspected routes, not exhaustive coverage or native
parity evidence; no emulator or self-hosting run was performed.

| Boundary | Rust | Corresponding native implementation |
|---|---|---|
| Candidate selection | [Selector bridge](../../crates/opforge-vm/src/execution_model/selector_bridge.rs) invokes family resolvers. The Intel8080/Z80 route can prepare encoded operand bytes in Rust before VM emission. | [Selection service](../../native/motorola68000/amigaos/tkpkg/tkpkg_selection_service.asm), `noFallback`, retries MSEL package rows once without the inferred shape filter. This is a different fallback strategy; the compact-selector route is separate. Candidate order, acceptance and failure behavior need comparison. |
| Common operand plans | [Plan execution](../../crates/opforge-vm/src/execution_model/selector_encoding.rs) interprets shared plans but also has an m65816-specific fallback. | [Operand runtime](../../native/motorola68000/amigaos/tkpkg/tkpkg_operand_runtime.asm) dispatches fixed tags such as `u8`, `u16`, `rel8` and `pair_u8_rel8`. These are native implementations of package-selected contracts, not arbitrary bytecode. Their adequacy depends on the contract, not the label “VM”. |
| Operand surfaces | Rust uses family parsers, including the [m68k surface parser](../../crates/opforge-families/src/m68k/operand_surface.rs). | The operand runtime retains a “transitional native seam”: local shape mappings, single-`a` accumulator recognition and `x`/`y` suffix rules. These assumptions need an ownership check; package selection alone does not make them generic. |
| Final emission | [Runtime core](../../crates/opforge-vm/src/runtime_model_core.rs) can feed prebuilt candidate bytes into package emission; m68k also uses richer semantic programs. | [Encode service](../../native/motorola68000/amigaos/tkpkg/tkpkg_encode_service.asm), `tkpkgEncodeExecuteSemanticProgramV2`, genuinely interprets literal, scalar and field operations. This does not establish equivalence of the earlier selection and preparation. |
| Parsing and expressions | Generic tokenizer/parser execution coexists with host helpers and operand-surface callbacks. | [Tokenizer](../../native/motorola68000/amigaos/tkvm/tkvm_runtime.asm) and [parser](../../native/motorola68000/amigaos/prvm/prvm_runtime.asm) execute bytecode. However, the [expression bridge](../../native/motorola68000/amigaos/opcore/opcore_expr_bridge.asm) selects a local EXVM v1 program and compiles expression text in native routines before ExprVM evaluation. Real VM evaluation does not establish Rust EXVM v2 parser parity. |

No Rust-host encoding call was observed on this native route. Native symbol
resolution callbacks must not be confused with target-specific encoding callbacks.
Likewise, `vm-runtime-only` is not proof that Rust avoids family handlers, and
`OPFORGE_TOKENIZER_FORCE_GENERIC` only controls tokenizer specialization. Existing
architecture-guard success does not prove semantic independence of these paths.

## B1 findings

`OPFORGE_TARGET_CALLBACKS=report|refuse` now instruments the four identified host
boundaries, with sticky refusal through candidate/parser recovery and a check
before binary publication. The [bounded runner](../../scripts/performance/package_boundaries.py)
compares identical baseline/report/refuse inputs; [usage and limits](../performance/vm-efficiency.md#target-callback-boundary-probe)
are maintained with the measurement tooling.

| 32-block case (160 instructions) | Host callback attempts, complete assembly | First refused boundary |
|---|---:|---|
| 6502 | 320 family candidate resolver calls | `lda`: family candidate resolver |
| Z80 | 320 family candidate resolver calls | `mvi`: family candidate resolver |
| 68000 | 226 family operand-surface parser calls | `.cpu`: family operand-surface parser |

6502/Z80 consult the resolver once per instruction per pass, including NOP.
68000's parsing calls include `.cpu`, `.org`, `.word` and `.long`; this is broader
than complex instruction addressing syntax. Its instruction emission in this
workload does not hit the family candidate resolver. At 8/128 blocks the respective
counts are 80/1280, 80/1280 and 58/898. Attempts include helpers that decline input;
these numbers do not establish that every consultation is necessary. Existing
32-block VM dispatch totals remain 6,476 / 6,732 / 7,020 in automatic tokenizer
mode; the callback work is a separate category, not additional VM instructions.

**B2 decision:** start by checking whether shared package-described operand
classification can replace family-parser consultation for scalar/register operands
and ordinary directive expressions. Compare ASTs, spans, errors and package-defined
surface precedence across targets before retaining a change. Do not treat skipping
callbacks until these benchmarks pass as semantic equivalence, or promise that one
operation will remove both the parsing and candidate-resolution dependencies.
The bounded batch completed in 2.40 seconds (29.45 seconds separately reported
cached build/setup). All 18 baseline/report positive runs matched independent
bytes, nine strict runs exited 1 without a binary, and 18 negative companions
preserved diagnostics. The callback-free package NOP control succeeded in refuse
mode. Detailed artifacts are local in ignored `build/b1-complete`; failed
experimental batches remain explicitly incomplete. Validation passed: 58 shared-type
and 416 VM library tests, the focused engine refusal/output test, 38 performance-tool
tests, scoped Clippy, formatting and workflow/architecture guards. No full-workspace
qualification is claimed. The native scan above remains source-only; B1 supplies Rust-side
evidence only.

## Bounded steps

1. **B1 — Make the boundary observable.** Add an opt-in Rust diagnostic mode that
   reports target-semantic callbacks and can refuse them at their invocation.
   Run the existing bounded 6502, Z80 and 68000 workloads; distinguish callback
   attempts, shared descriptor/helper work, bytecode execution and specializations.
   Do not ban ordinary Rust implementation of generic VM operations. Deliver a
   reproducible report of the first unsupported package-controlled boundary per
   route, plus focused checks that refusal cannot silently fall back. Compare
   normal output/diagnostics with the existing baseline. A blocked strict run is
   useful evidence, not a performance or correctness result.

2. **B2 — Replace one demonstrated dependency.** Review B1 and select one shared
   operation. First check whether existing package data or native execution already
   supplies the needed semantics; do not invent duplicate contracts. Implement a
   package-controlled Rust path and compare against the retained working reference
   on identical inputs, including relevant errors, unresolved values, operand
   spans, selection priority and pass/layout changes. Include representative cases
   from more than one target where the operation applies. Deliver a reviewable
   implementation with measured total preparation/execution cost and memory impact;
   revise or stop if the complexity is not justified.

3. **B3 — Check the same operation on native.** Map the exact Rust input, output and
   state contract to its native boundary. Determine whether native already executes
   it generically, needs a shared operation, or retains a semantic shortcut. Make
   only the agreed coherent change, removing the responsibility it replaces rather
   than growing the selection monolith. Validate small complete cases, including
   the relevant fallback behavior, under the [native parity contract](../../agents/rules/native-rust-parity-porting.md).
   Source inspection and localization probes cannot substitute for actual native
   parity; label evidence by proof level. Deliver a bounded testable result, or
   name the precise unsupported invariant without claiming completion.

Each step is a separate reviewable outcome; B1 findings determine B2/B3 scope.
Aim for 30–60 minutes and roughly 15k agent tokens per iteration. Reuse existing
measurement tooling: at most 60 seconds per invocation and five minutes per batch.
Reduce workload size when necessary; never extend into full native self-hosting.
Report incomplete runs honestly. Bytecode counts alone do not price descriptors,
callbacks, decoding, allocations or older-hardware memory access.

Resume tokenizer or prepared-selection optimization after reviewing these boundary
findings. No all-family migration, package-format redesign or runtime-package
compiler is assumed. Keep this plan current; transfer durable results into
maintained documentation and remove it when complete. Git retains the history.
