# Package-controlled execution: Rust and native

Status: B2 complete and locally validated; review before B3 native work. Companion to [the runtime reset](native-runtime-reset.md),
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
Before 1.0 there is only the latest supported state of each VM/bytecode contract;
version identifiers reject mismatches, not select retained old executors. Migrate
package generation and Rust/native consumers together when changing a contract.
Temporary differential oracles belong in ignored experiment output or Git history,
not permanent legacy bytecode paths. This does not waive assembly-source compatibility.

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

## B1 audit boundary

B1 added `OPFORGE_TARGET_CALLBACKS=report|refuse` at four host boundaries, with
sticky refusal through candidate/parser recovery and a check before binary
publication. It exposed family-parser consultation even for `.cpu`, `.org` and
data directives, leading to B2 below. The [bounded runner](../../scripts/performance/package_boundaries.py)
remains the reproducible probe; [usage and limits](../performance/vm-efficiency.md#target-callback-boundary-probe)
are maintained with the measurement tooling. B1's detailed baseline is in Git.

## B2 result: shared operand grammar

Generic directives are not a CPU/family feature. `.byte`, `.word` and `.org` must
stay in shared core processing across targets, without target-specific operand
parsers or instruction encoders. The existing parser opcodes now select core versus
instruction grammar explicitly;
that state follows expression requests and parser checkpoints. Directive operands
and single literal/identifier/register instruction operands use the shared
expression contract without family consultation. Existing immediate syntax such as
`.byte #1` is preserved. Compound instruction surfaces retain their existing
extension route. No bytecode format/version or package contents changed, and no
legacy executor or compatibility switch was added.

Dot-prefixed statement names belong to shared directive/macro/segment handling.
A final dispatch check also closes an existing hole: unresolved dot names now
produce a shared directive error even when no typo suggestion exists, instead of
falling through to CPU instruction processing. Cross-target refusal-mode tests
cover unknown names and dot-prefixed instruction spellings. The older Rust parser's
dot-statement path now uses shared expression grammar too, removing an AST
lockstep disagreement for grouped directive expressions.

All nine scale cases preserve output and diagnostics. Across 21 differential cases,
19 match bytes and exact normalized diagnostics/spans; two rejected inputs now use
core-expression diagnostics: `.byte (1,2)` reports an unexpected comma rather than
an unsupported instruction tuple, and `.word (a0)+` reports an incomplete expression
rather than an undefined register-named symbol. Neither produces output. These are
reviewed corrections to invalid-input diagnostics, not successful equivalence cases.

68000 family-parser attempts fell 58→8, 226→32 and 898→128 at 8/32/128 blocks. The
remaining calls are compound instruction operands. Directive-only probes complete
in strict mode with zero calls. 6502/Z80 resolver counts and all three VM dispatch
totals are unchanged. Three unprofiled 32-block samples per side showed no consistent
speedup: medians were 36.59→40.96 ms, 37.77→37.47 ms and 38.49→37.68 ms respectively.
Do not infer native performance from this small process-level sample.

The measured operand-routing snapshot, before the final unknown-dot diagnostic fix,
kept canonical package and release executable sizes unchanged (368,579 and
4,324,864 bytes). The implementation adds a small grammar tag to parser state and
no heap cache. One unprofiled 68000/32 paired process-memory probe using fresh-worker
`RUSAGE_CHILDREN` measured 18,087,936 versus 16,875,520 peak RSS bytes; this is noisy
whole-Rust-process evidence, not an attributable saving or a native memory estimate.
The alternative `/usr/bin/time` RSS probe was unavailable under the host sandbox.

Validation: 200 core library tests; two cross-target dot-statement/Rust-VM lockstep
tests; 15 directive-typo tests; 419 VM library tests; 10 parser parity tests; 10 Rust-side native-ABI
contract tests (not real-native proof); one actual VM-work counter test; 42 performance-tool tests; scoped
Clippy, formatting and workflow guards. Expanding Clippy to all assembler test
targets exposed four existing warnings in untouched native smoke/parity test code
(`large_enum_variant`, two `manual_range_patterns`, and `useless_vec`); that broader
check is not clean. The bounded B1 runner also passed. Results
remain under ignored `build/b2-*`; reproducible comparison commands live in the
[measurement guide](../performance/vm-efficiency.md#comparing-operand-routing-changes).
B3 should now compare core/instruction grammar ownership on native, preserving
these boundaries instead of porting unnecessary family consultations.

## Bounded steps

1. **B1 — Make the boundary observable (complete).** Add an opt-in Rust diagnostic mode that
   reports target-semantic callbacks and can refuse them at their invocation.
   Run the existing bounded 6502, Z80 and 68000 workloads; distinguish callback
   attempts, shared descriptor/helper work, bytecode execution and specializations.
   Do not ban ordinary Rust implementation of generic VM operations. Deliver a
   reproducible report of the first unsupported package-controlled boundary per
   route, plus focused checks that refusal cannot silently fall back. Compare
   normal output/diagnostics with the existing baseline. A blocked strict run is
   useful evidence, not a performance or correctness result.

2. **B2 — Replace one demonstrated dependency (complete).** Review B1 and select one shared
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
