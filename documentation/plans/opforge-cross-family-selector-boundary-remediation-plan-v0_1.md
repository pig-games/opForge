<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->

# opForge Cross-Family Selector Boundary Remediation Plan v0.1

## Metadata

- Source: user instructions in this conversation to extend the MOS-family selector-boundary fix into a cross-family analysis and create a plan for removing similar remaining hard boundaries; the active worktree `AGENTS.md`; the plan-authoring skill in `skills/opforge-plan-authoring/SKILL.md`; the plan template in `templates/plan-template.md`; the current expr-resolver registration in `crates/opforge-vm/src/execution_model.rs`; the remaining Intel 8080 / 8085 / Z80 selector synthesis in `crates/opforge-vm/src/execution_model/selector_bridge.rs`; the remaining M65816-specific selector encoding helpers in `crates/opforge-vm/src/execution_model/selector_encoding.rs` and `crates/opforge-vm/src/selector_encoding_utils.rs`; the generic `.opasm` M68K operand-shape gate in `crates/opforge-vm/src/vm_opasm.rs`; the existing family-owned candidate generation in `crates/opforge-families/src/m6800/module.rs` and `crates/opforge-families/src/mos6502/selector.rs`; and the current native Motorola 68000 transitional selector runtime in `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm`
- Mode: `remediation`
- Owner: GitHub Copilot
- AGENTS binding: The active worktree `AGENTS.md` rules remain binding during execution.

## Goal

Remove the remaining family-specific selector, selector-operand, and operand-shape
boundaries that still leak into generic runtime paths after the completed MOS
selector normalization fix, while preserving the distinction between:

- allowed package-authoring and builder logic that knows CPU or family tables,
- allowed family-owned parser and normalizer logic, and
- forbidden generic runtime logic that derives family semantics from raw source
  syntax, family mnemonics, family register spellings, shape strings, or
  family-specific operand-plan branches.

This plan also classifies the current Motorola 68000 situation precisely:

- there is not currently a same-class 680x0 leak in the generic Rust selector
  bridge because no Motorola 68000 family expr selector resolver is registered
  there yet,
- there is a related generic parser-boundary debt in the `.opasm` VM parser,
  where M68K operand-shape admission and mnemonic-specific shape parsing still
  live in `crates/opforge-vm/src/vm_opasm.rs`, and
- there is a stronger transitional boundary debt in the native Motorola 68000
  tkpkg selector service, where plan tags and shape tags are matched and
  executed through hardcoded assembly branches instead of a fully generic
  package-driven selector runtime.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Scope is limited to selector-boundary, selector-operand, and closely related
  operand-shape runtime ownership seams. Do not widen this plan into unrelated
  parser cleanup, family table redesign, or speculative package-format work.
- Builder and package-authoring logic in `crates/opforge-vm/src/builder.rs` may
  remain family-aware when generating package descriptors or VM programs. This
  plan targets generic runtime execution and parsing paths, not selector-table
  compilation.
- Work items must prefer the smallest viable ownership move: route through an
  existing family seam before inventing a new schema or VM contract.
- The existing MOS-family fix is the reference pattern: family-owned operand
  normalization feeds generic runtime candidate selection.
- The Motorola 6800 family resolver path is not currently a blocking finding in
  this plan because its operand resolution and `vm_encode_candidates` surface is
  already family-owned; only dispatch remains in the VM resolver registration.
- Native Motorola 68000 work items must load `agents/rules/native-68000.md` and
  include the native formatter gate.
- No plan item may mark the boundary closed merely by moving hardcoded strings
  from one generic runtime file to another.
- This plan must not become active until `plan-compliance-reviewer` returns
  `PASS`.

## Version Impact

- Affected component(s): `crates/opforge-vm`, `crates/opforge-families`, package
  selector descriptor handling, runtime boundary documentation, runtime tests,
  and native Motorola 68000 tkpkg selector service code
- Impact class: patch
- Owned contract: generic VM runtime may dispatch through family-owned parser or
  selector adapters and may execute generic package-owned selector data, but it
  must not derive CPU-family selector semantics from family-specific source
  syntax, shape tags, mode names, register spellings, or CPU-state rules
- Rationale: the MOS-family fix removed one concrete boundary leak, but the same
  ownership problem still exists in the Intel family runtime path, the M65816
  selector encoder path, the generic M68K operand parser gate, and the native
  Motorola 68000 tkpkg selector implementation

## Findings Inventory

- F1: `crates/opforge-vm/src/execution_model/selector_bridge.rs` still performs
  Intel 8080 / 8085 / Z80-specific selector derivation and candidate synthesis,
  including CPU-id branching, condition stripping, Z80 indexed and CB forms, and
  interrupt-mode handling in a generic runtime file.
- F2: `crates/opforge-vm/src/execution_model/selector_encoding.rs` and
  `crates/opforge-vm/src/selector_encoding_utils.rs` still execute M65816-only
  selector plans and bank-width semantics through hardcoded runtime plan names,
  shape checks, mnemonic lists, and direct `families::m65816::state` access.
- F2a: the immediate blocking M65816 ownership leak is the direct generic-runtime
  execution of M65816-only state and decision rules.
- F2b: after the adapter-only remediation, any remaining M65816-only selector
  plan vocabulary in generic runtime code becomes a separate follow-up closure
  item rather than part of the first extraction commit.
- F3: `crates/opforge-vm/src/vm_opasm.rs` still hardcodes Motorola 68000 family
  operand-shape admission and mnemonic-specific operand parsing in the generic
  `.opasm` VM parser.
- F4: `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm` still hardcodes
  selector plan tags and shape tags such as `rel8`, `pair_u8_rel8`,
  `immediate`, `direct_x`, and `direct_y`, then executes plan behavior through a
  manual plan switchboard and shape-specific stripping helpers.
- F4a: the first native closure slice is the hardcoded plan-tag switchboard for
  generic plan dispatch.
- F4b: the second native closure slice is the remaining hardcoded shape and
  pair-specific operand handling.
- F5: The repo currently lacks cross-family guardrails that distinguish allowed
  family-owned selector parsing from forbidden generic-runtime selector
  derivation in both Rust and native transitional paths.

## Planning Decisions Captured Up Front

- The next Rust runtime slice should target F1 first because it is the closest
  structural match to the fixed MOS leak and already sits behind the existing
  `FamilyExprResolver` seam.
- F2 should be split between a near-term adapter move and a later generic
  selector-plan surface cleanup, instead of attempting a package-format redesign
  in one step.
- F3 is a parser-boundary issue, not a selector-bridge issue. It belongs in the
  same remediation plan because it reflects the same ownership mistake in a
  neighboring generic runtime layer.
- F4 is real 680x0 debt, but it is transitional debt in the native package-
  backed runtime rather than evidence that the Rust selector bridge already has a
  Motorola 68000 resolver leak, so native closure should also happen in more than
  one commit-sized slice.
- The Motorola 6800 family path is currently the positive reference for this
  plan: family parsing and `vm_encode_candidates_for_operands` already live in
  family code, so that slice should inform the Intel-family and later M68K
  ownership moves.

## Work Items

- [x] Item 1: move Intel 8080 / 8085 / Z80 selector synthesis behind a family-owned expr resolver
  - Source requirement or finding IDs: F1; existing `FamilyExprResolver` seam in `crates/opforge-vm/src/execution_model.rs`; completed MOS-family selector normalization fix as the ownership pattern to mirror.
  - Expected files:
    - `crates/opforge-vm/src/execution_model/selector_bridge.rs`
    - `crates/opforge-vm/src/execution_model.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-families/src/intel8080/**` and possibly `crates/opforge-families/src/i8085/**` or `crates/opforge-families/src/z80/**`
    - `crates/opforge-vm/src/intel8080_vm.rs` only if builder-only helpers must be separated from runtime helpers
  - Full quality gates:
    - `cargo test -p vm execution_model_intel_expr_ -- --nocapture`
    - `cargo test -p vm execution_model_tokenizer_auto_mode_uses_vm_for_intel8080_family -- --nocapture`
    - `cargo test -p vm execution_model_assembler_tokenization_path_uses_vm_for_intel8080_family -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to replacing generic Intel-family selector derivation with family-owned candidate generation and adding a matching guardrail test
  - Commit outcome:
    - generic selector bridge code no longer branches on `8085` or `z80` to derive Intel-family selector candidates; those decisions live behind family-owned resolver code
  - Definition of done:
    - the generic bridge keeps only family dispatch and generic candidate plumbing
    - Z80 half-index, indexed-CB, indexed-memory, interrupt-mode, and indirect-LD behaviors remain covered by focused tests
    - a new guardrail fails if Intel-family selector vocabulary or CPU-id branching is reintroduced into the generic selector bridge

- [x] Item 2: move M65816 state and decision ownership behind one adapter seam
  - Source requirement or finding IDs: F2a; M65816 selector-plan handling in `crates/opforge-vm/src/execution_model/selector_encoding.rs`; M65816 helper and state logic in `crates/opforge-vm/src/selector_encoding_utils.rs`.
  - Expected files:
    - `crates/opforge-vm/src/execution_model/selector_encoding.rs`
    - `crates/opforge-vm/src/selector_encoding_utils.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-families/src/m65816/**`
  - Full quality gates:
    - `cargo test -p vm execution_model_encodes_m65816_ -- --nocapture`
    - `cargo test -p vm execution_model_vm_encode_supports_m65816_cpu_tables -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to moving M65816-only width and bank decisions behind one family-owned adapter seam without broadening into parser work or package-schema changes
  - Commit outcome:
    - generic selector encoding no longer reads M65816 state or executes M65816-only decision rules as open-coded runtime branches
  - Definition of done:
    - generic selector encoding delegates M65816-only state and decision rules through one family-owned adapter seam
    - current M65816 immediate-width, force-suffix, long/absolute fold, and bank-diagnostic behavior remains covered by focused tests
    - a guardrail test prevents reintroducing direct `families::m65816::state` access in generic encoding helpers

- [x] Item 3: close any remaining M65816-only selector-plan vocabulary still left in generic runtime code
  - Source requirement or finding IDs: F2b; remaining M65816-only plan names or shape checks that survive Item 2.
  - Expected files:
    - `crates/opforge-vm/src/execution_model/selector_encoding.rs`
    - `crates/opforge-vm/src/selector_encoding_utils.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-families/src/m65816/**`
    - `crates/opforge-package/**` only if a minimal generic operand-plan descriptor extension is proven necessary by Item 2
  - Full quality gates:
    - `cargo test -p vm execution_model_encodes_m65816_ -- --nocapture`
    - `cargo test -p vm execution_model_vm_encode_supports_m65816_cpu_tables -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the residual M65816 selector-plan vocabulary after Item 2, with any package-surface change justified as the smallest remaining unblocker
  - Commit outcome:
    - generic runtime no longer contains residual M65816-only selector-plan vocabulary beyond explicitly documented generic primitives
  - Definition of done:
    - any remaining M65816-only plan name or shape gate in generic runtime code is either removed, generalized, or explicitly moved behind a family-owned seam
    - package-surface changes are absent unless Item 2 proves them necessary
    - guardrails distinguish temporary adapter delegation from fully generic operand-plan execution

- [x] Item 4: move Motorola 68000 operand-shape admission out of the generic `.opasm` VM parser
  - Source requirement or finding IDs: F3; generic M68K operand-shape gate in `crates/opforge-vm/src/vm_opasm.rs`.
  - Expected files:
    - `crates/opforge-vm/src/vm_opasm.rs`
    - `crates/opforge-vm/src/runtime_tests.rs` or `crates/opforge-vm/src/execution_model/tests.rs`
    - `crates/opforge-families/src/m68k/**` or a new family-owned operand-shape adapter module
  - Full quality gates:
    - `cargo test -p vm m68k_ -- --nocapture`
    - `cargo test -p asm m68k_ -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to replacing family-gated M68K operand-shape parsing in the generic parser with a family-owned hook or adapter
  - Commit outcome:
    - generic `.opasm` parsing no longer decides whether Motorola 68000-specific shapes such as postincrement, predecrement, tuple forms, TEX forms, and bitfield suffixes are legal through hardcoded family checks in `vm_opasm.rs`
  - Definition of done:
    - the generic parser keeps only generic operand-splitting and hook invocation
    - Motorola 68000 operand-shape support remains behaviorally intact for the current M68K family corpus
    - parser-layer ownership is documented separately from selector-layer ownership so later audits do not confuse the two

- [x] Item 5: replace the native Motorola 68000 tkpkg generic plan-tag switchboard with a narrow package-driven dispatch slice
  - Source requirement or finding IDs: F4a; native transitional selector code in `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm`; native Motorola 68000 rule pack in `agents/rules/native-68000.md`.
  - Expected files:
    - `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm`
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - `cargo test -p asm motorola68020_item6_3_ -- --nocapture`
    - `cargo test -p asm motorola68020_item6_7_ -- --nocapture`
    - `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to replacing the plan-tag switchboard for generic plan dispatch without touching the remaining shape helpers beyond what is strictly required
  - Commit outcome:
    - the native tkpkg service no longer dispatches generic selector plans through a hardcoded linear plan-tag switchboard
  - Definition of done:
    - generic plan dispatch is expressed through package-driven generic logic or one narrow shared helper layer rather than per-tag assembly branches
    - existing native parity tests for single-operand and branch-plan paths remain green
    - any remaining shape-specific helper debt is left intentionally for Item 6 and documented as such

- [x] Item 6: close the remaining native Motorola 68000 shape-specific selector helpers
  - Source requirement or finding IDs: F4b; hardcoded shape helpers and pair-specific operand handling in `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm`.
  - Expected files:
    - `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm`
    - `native/motorola68000/amigaos/tkpkg/**/*.asm` if helper extraction is needed
    - `crates/opforge-asm/src/tests.rs`
    - `documentation/architecture/cpu-specific-arch-boundary.md`
  - Full quality gates:
    - `cargo test -p asm motorola68020_item6_7_ -- --nocapture`
    - `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the remaining shape-specific native selector helpers after Item 5 lands
  - Commit outcome:
    - the native tkpkg service no longer hardcodes remaining shape tags and pair-specific operand handling as an implicit selector ownership boundary
  - Definition of done:
    - remaining shape-specific helpers for immediate, accumulator, indexed suffixes, paren modes, and pair-direct handling are removed, generalized, or explicitly isolated behind one documented transitional seam
    - existing native parity tests for rel8, pair-direct bit-branch, and selected encode paths remain green
    - the architecture documentation records any residual native exception honestly if one still remains

- [ ] Item 7: add cross-family guardrails and close the documentation gap
  - Source requirement or finding IDs: F5; architectural boundary clarification requested in this conversation.
  - Expected files:
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-asm/src/tests.rs` if native guardrails are text-based or fixture-based
    - `documentation/architecture/cpu-specific-arch-boundary.md`
    - `documentation/plans/opforge-cross-family-selector-boundary-remediation-plan-v0_1.md` only for checkbox bookkeeping
  - Full quality gates:
    - `cargo test -p vm generic_selector_runtime_ -- --nocapture`
    - `cargo test -p vm vm_opasm_family_gate_ -- --nocapture`
    - `cargo test -p asm motorola68020_tkpkg_ -- --nocapture` when native guardrails are touched
    - `scripts/workflow/run_rust_quality_gate.sh`
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-cross-family-selector-boundary-remediation-plan-v0_1.md`
    - `python3 scripts/workflow/check_workflow_artifact_bundle.py plan documentation/plans/opforge-cross-family-selector-boundary-remediation-plan-v0_1.md`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to final guardrails and documentation after the implementation slices land
  - Commit outcome:
    - the repo has explicit enforcement and documentation describing which files may remain family-aware and which generic runtime files may not
  - Definition of done:
    - guardrails distinguish allowed family-owned parser or selector adapters from forbidden generic-runtime family derivation
    - the documentation explicitly records the 680x0 classification: no current Motorola 68000 selector-bridge resolver leak, but active parser-boundary and native transitional selector debts remain

## Blocking Rules

- no commit before all quality gates pass
- `plan-compliance-reviewer` must return `PASS` before commit
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- archive completed plans with `scripts/workflow/archive_completed_plan.sh`
