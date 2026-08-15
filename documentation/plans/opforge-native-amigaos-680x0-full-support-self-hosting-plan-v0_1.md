<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# Plan: Native AmigaOS 680x0 Full Support and Self-Hosting

## Metadata

- Source: User requests on 2026-08-14 to plan full Rust-authoritative 680x0
  support for the native 68020/AmigaOS opForge implementation and to make the
  final test a complete native self-build using the same logical CLI command as
  the Rust build, with only Unix/AmigaOS path notation allowed to differ; later
  clarifications require full Hunk/S-record output, unmodified Rust-built
  single/combined packages, a package-first Rust cutover, CPU-neutral VM/package
  improvements, adoption of every applicable improvement by every existing CPU
  family, and a green unfiltered Rust workspace test corpus covering them all.
- Mode: implementation
- Owner: opForge implementation agent
- Base commit audited: `3d136376a941cd981d155df5411388388ed579cc`
- Worktree: `worktrees/native-680x0-amigaos-plan` on
  `codex/native-680x0-amigaos-plan`
- AGENTS binding: The active worktree `AGENTS.md` rules remain binding during execution.
- Workflow: authored with `opforge-plan-authoring`; validate with
  `scripts/workflow/run_plan_workflow.sh` and `make workflow-gate`.
- Validation status: Draft until the companion quality-gate file records a
  fresh branch-local `plan-quality-reviewer` `PASS`.

## Goal

Make the 68020/AmigaOS-native `opforge` executable assemble the complete 680x0
surface that the Rust implementation supports, using the Rust implementation as
the semantic oracle and package-owned CPU/family/dialect data as the portable
delivery mechanism. Completion means exact success and diagnostic parity for
the applicable Motorola 68000 reference corpus and a fail-closed Level D test
in which the native executable completely rebuilds itself.

Before native 680x0 work, improve the shared package format and VM so complete
680x0 semantics execute from serialized packages through CPU-neutral operations.
Those improvements must preserve all registered families, be adopted by 65x02
and every other existing family where applicable, and reduce the work required
to add future CPU families.

“Full 680x0” in this plan means the Rust-visible profiles present at the audited
base: `m68000`, `m68010`, `m68020`, `m68030`, `m68040`, and `m68080`, including
the shipped `68881`, `68882`, integrated `68040`, and `68080` FPU selections,
the shipped narrow MMU surface, the Motorola dialect aliases, negative legality
contracts, and 68080 Apollo/AMMX behavior. It does not invent `m68060` or any
instruction, MMU, FPU, dialect, or output behavior absent from the Rust oracle.

The native executable must consume the exact serialized package bytes produced
by the Rust package builder. No native-specific recompilation, rewriting,
repacking, metadata sidecar, or postprocessing is permitted. This contract must
hold for a 680x0-only package, a 65x02-only package, and a combined package that
contains both families.

The executable's host architecture remains `.cpu 68020`; target `.cpu`
selection controls the package pipeline and must never be confused with the
machine on which the native executable runs.

## Source Requirements

- `N68X0-001`: Rust is authoritative for accepted source, emitted bytes,
  selected widths, diagnostics, CPU/FPU/Apollo legality, listings, and Hunk
  output.
- `N68X0-002`: The native runtime supports every audited Rust target profile:
  `m68000`, `m68010`, `m68020`, `m68030`, `m68040`, and `m68080`, including
  aliases already registered by Rust.
- `N68X0-003`: CPU/family/dialect/register/addressing/instruction semantics stay
  in package/family definitions. Generic Rust VM, native VM, workflow, and CLI
  paths may gain architecture-neutral opcodes and records only.
- `N68X0-004`: The package contract carries every operand, selector, legality,
  state, and encoder operation needed by 680x0; the native implementation must
  not reproduce the Rust m68k handlers as hard-coded generic CLI logic.
- `N68X0-005`: Multi-pass sizing, symbol stability, branch widening,
  big-endian extension emission, PC-relative evaluation, and deterministic
  failure behavior match Rust.
- `N68X0-006`: `.cpu`, `.fpu`, and `.apollo` state transitions and their
  diagnostics match Rust and reset between files/runs.
- `N68X0-007`: Every applicable source and reference artifact under
  `examples/motorola68000/**` and `examples/reference/motorola68000/**` is
  covered or has a reviewed, concrete exclusion. Exclusions cannot hide a
  missing 680x0 feature owned by this plan.
- `N68X0-008`: Every parity slice has a machine-readable
  `native-rust-parity` record, named Rust/native boundary, proof levels A-E,
  and explicit “proves / does not prove” statements.
- `N68X0-009`: Required FS-UAE proofs obey the singular fail-closed Level D
  contract: actual-case in-memory Rust oracle, fresh challenge, exact start and
  completion responses, explicit guest exit, exact comparison, continued
  execution after an earlier failure, and unconditional ephemeral cleanup.
- `N68X0-010`: The native CLI implements the complete Rust-supported Amiga Hunk
  executable surface and Motorola S-record surface, including CLI and
  source-declared output selection, section ordering, addresses, relocations,
  memory attributes, record/checksum/termination rules, deterministic
  diagnostics, and simultaneous listing/Hunk/S-record output, without a
  Rust-side postprocessor. “Full Hunk” is bounded by the Rust executable-Hunk
  contract; Hunk objects/libraries, overlays, debug hunks, and symbol hunks
  remain excluded because the Rust contract excludes them.
- `N68X0-011`: The final self-host test uses the same ordered logical CLI argv
  for Rust and native builds. Only path operands may be translated between
  Unix and AmigaDOS notation; flags, ordering, CPU, package, inputs, module
  roots, include roots, and requested outputs may not differ.
- `N68X0-012`: A Rust-built generation-0 native executable builds generation 1;
  generation 1 is launched and builds generation 2 with the same logical
  command; both generations match the same-command Rust oracle byte-for-byte
  for Hunk and S-record output and under the approved deterministic
  normalization for listing text.
- `N68X0-013`: Native package loading consumes unmodified, byte-identical Rust
  package-builder output. A Rust-produced 680x0-only package assembles the full
  680x0 surface, a Rust-produced 65x02-only package retains the complete
  supported native 65x02 surface, and a Rust-produced combined package selects
  and switches between both families without ordering dependence, semantic
  leakage, rewriting, or a native-only package variant.
- `N68X0-014`: Before native 680x0 production work begins, the Rust package
  path must carry the complete supported 680x0 operand, selection, legality,
  directive-state, encoding, fixup, pass-stability, and diagnostic contract.
  After package construction, packaged Rust assembly must execute without any
  direct call into an m68k family parser, selector, legality, or encoder
  callback. Family callbacks may remain only as package-build inputs and as a
  temporary differential oracle until the package-first completion gate passes.
- `N68X0-015`: Every new VM operation, package record, codec feature, and native
  interpreter operation must be CPU/family/dialect neutral wherever the
  underlying concept is reusable. It must improve or preserve the common
  package runtime for all registered CPU families and make future families
  composable without native runtime edits. A concept that cannot be made
  neutral remains family-owned package data or a composition of neutral
  operations; it must not become a nominally generic m68k-shaped opcode.
- `N68X0-016`: Every package/VM improvement introduced by this plan must be
  audited against every CPU family registered at execution time. Each family
  adopts every applicable neutral improvement in its Rust-built package, with
  unchanged source semantics and diagnostics; inapplicability requires a
  concrete family-semantic reason. Because 65x02 already has native AmigaOS
  support, every later native VM implementation additionally runs its retained
  complete proof so 680x0 enablement cannot leave that native family on a stale
  package path.
- `N68X0-017`: Rust package-first completion requires the canonical unfiltered
  root `cargo test --locked` run inside
  `scripts/workflow/run_rust_quality_gate.sh` to pass across every workspace
  crate and every supported CPU family. The plan may not satisfy this with
  package-local filters, selected-family tests, new ignores/skips, workspace
  exclusions, baseline waivers, or a reduced default-member set.

## Authority and Baseline

The implementation must read the current Rust behavior directly. The primary
authorities are:

- `crates/opforge-families/src/m68k/**` and the `m68000`, `m68010`, `m68020`,
  `m68030`, `m68040`, and `m68080` CPU modules;
- `crates/opforge-vm/src/builder.rs`, `vm_opasm.rs`, and
  `execution_model/**` for package materialization, operand surfaces,
  selection, and encoding;
- `crates/opforge-package/src/package.rs` and `package/codec/**` for `.opcpu`
  wire ownership, plus `build_hierarchy_package_from_registry` and registry
  composition tests for byte-identical single-family and combined packages;
- `crates/opforge-asm/src/asmline*.rs`, `opasm.rs`, and the 680x0 tests in
  `crates/opforge-asm/src/tests.rs` for assembler behavior;
- `crates/opforge-asm/src/output.rs`, the Hunk/output model implementation, the
  S-record writer, `documentation/opForge-amiga-hunk-full-support-spec-v0_2.md`,
  and `documentation/opForge-amiga-hunk-executable-completeness-spec-v0_3.md`
  for the complete Rust-supported executable-Hunk and S-record contracts;
- the accepted behavior in
  `documentation/opForge-m68000-family-68010-68020-68030-68040-cpu-extension-spec-v0_1.md`,
  `documentation/opForge-m68000-family-68020-68030-68040-mmu-fpu-extension-spec-v0_1.md`,
  `documentation/opForge-m68000-family-residual-risks-spec-v0_1.md`, and
  `documentation/opForge-m68000-family-68080-full-extension-spec-v0_1.md`;
- `examples/motorola68000/**` and matching references as the initial corpus,
  with live same-source Rust assembly—not stored filenames—as the Level D
  oracle.

At the audited base, the default package identifies all six target CPUs, but
`crates/opforge-vm/src/builder.rs` materializes selector/encoder programs only
for other family paths; Rust m68k operand parsing and encoding still call
family handlers directly. Native `tkpkg` selection/operand code currently
recognizes a bounded MOS-oriented selector surface. This is the first
architectural divergence to remove. The native package store is fixed at
262,144 bytes and must be measured against the completed 680x0 package rather
than assumed sufficient.

## Activation Prerequisites

No work item may start until all of these are true on the execution branch:

1. `documentation/plans/opforge-native-cli-reference-parity-expansion-plan-v0_1.md`
   Items 8, 9, and 10 are complete, committed, and green, including required
   layout/output and diagnostic parity. This plan does not duplicate those
   open generic CLI responsibilities.
2. `scripts/workflow/run_native_cli_expansion_completion.sh` produces a valid
   clean-source completion receipt for its owned scope.
3. `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`,
   `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`, and
   `make workflow-gate` pass on the prerequisite merge.
4. The plan is updated if prerequisite completion changes a boundary, file
   owner, package version, CLI option, or canonical command named here; the
   update must pass `plan-quality-reviewer` again before implementation resumes.

## Constraints

- One active work item at a time; each item ends in exactly one focused commit.
- Before production edits, Items 1-13 record one package-phase boundary slice;
  Items 14-40 record the boundary contract required by
  `agents/rules/native-rust-parity-porting.md` and add/update exactly one native
  parity slice under `documentation/plans/slices/`.
- Load `agents/rules/native-68000.md` for native assembly, `agents/rules/fs-uae.md`
  for emulator work, `agents/rules/native-parity-failure-triage.md` after a
  parity failure, and `agents/rules/native-68000-safe-instrumentation.md` before
  adding any diagnostic, trace, assertion, or event.
- A moved failure, reduced fixture, prefix scan, or debug probe is proof Level E
  localization only. Each item closes one named invariant with focused Level
  B/C proof; Items 14-40 also require the named Level D confirmation.
- New portable package opcodes must be versioned, bounded, fuzz/negative tested,
  decoded fail-closed by Rust and native, and remain useful without embedding
  the spelling `m68k`, a register name, or an instruction name in generic
  interpreter control flow.
- Every proposed generic opcode/record must document its CPU-neutral semantic
  contract, operands, bounds, failure behavior, and applicability to existing
  or plausible future families. Where the concept applies to another registered
  family, at least one cross-family test must exercise it. Where it does not,
  review must confirm that family-owned data composed from narrower neutral
  operations is preferable to adding a family-shaped VM primitive.
- Package/VM changes must run round-trip, canonicalization, malformed-input,
  and behavior-regression coverage for every registered family/package fixture,
  not only 680x0 and 65x02. Existing package bytes remain stable unless the
  item explicitly versions the wire contract and records migration evidence.
- Items 1-13 must preserve the root workspace membership and may not add a
  `default-members` restriction, exclude an existing crate/family suite, mark a
  previously active regression test ignored, introduce a conditional skip, or
  waive a failure. Focused tests localize changes; only the complete unfiltered
  Rust quality gate can close the package-first phase.
- Package generation may compile existing Rust family semantics into portable
  data/programs. It must not fork a second hand-maintained encoding table.
- Item 12 is one mechanical all-family adoption/audit invariant. If adopting an
  operation for any family requires new family semantics, changes public
  behavior, or cannot remain a mechanical package-compiler migration, insert a
  separate reviewed family-owned work item before Item 12 can complete.
- Items 1-13 are a Rust/package-only enablement phase. They may not change
  `native/**` production code. Native implementation begins only after Item 13's
  completion receipt proves that serialized package execution has no runtime
  dependency on m68k family callbacks.
- After Item 13, the produced package schema and CPU-family semantic programs are
  immutable input to native work. A native parity failure must first be fixed
  in an architecture-neutral native package operation. If evidence instead
  shows that the package is incomplete, native work stops, the owning Item 1-13
  contract is reopened in a separate reviewed commit, and plan quality approval
  is renewed before native execution resumes.
- Native tests must stage the bytes emitted by the Rust package builder
  directly and attest their digest before and after transfer. A native-only
  package compiler, converter, rewriter, subset extractor, reordered package,
  auxiliary semantic file, or patched fixture is a test failure, not a bridge.
- Every native generic-runtime item must run the existing complete native 65x02
  completion wrapper against the current Rust-built 65x02-only package. A
  regression, skipped case, legacy-package substitution, or need for a separate
  65x02 native path blocks the item.
- No item may change Rust-visible 680x0 semantics merely to make the native port
  easier. A discovered Rust defect requires a separately approved source-spec
  amendment and focused fix before native parity continues.
- Reference updates must use an explicit allowlist and the
  `opforge-golden-reference-maintainer` workflow. Documentation/release notes
  follow `opforge-doc-sync-and-release-notes` only after behavior is final.
- Do not claim full support from representative smokes. Completion requires
  corpus accounting plus the self-host proof.

## Shared Full Quality Gate

Every native work item (Items 14-40) runs its named focused tests and Level D
command, then all of:

```sh
scripts/workflow/run_native_68000_format_gate.sh
python3 scripts/workflow/check_cpu_specific_arch_boundary.py
python3 scripts/workflow/check_native_runtime_boundary_inventory.py
python3 scripts/workflow/run_native_porting_quality_gate.py --staged
scripts/workflow/run_native_existing_parity_completion.sh --verify
RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh
make workflow-gate
```

If an item changes package codec/version behavior, it also runs focused
`opforge-package` round-trip, malformed-input, and canonicalization tests. If it
changes examples/references, it also runs
`cargo test -p asm examples_match_reference_outputs -- --nocapture`. Each
native item's Level D test uses the known-good FS-UAE environment from
`agents/rules/fs-uae.md` and `--test-threads=1`.

For Items 1-13, “Package Phase Quality Gate” means the item's focused tests plus
all of:

```sh
cargo test -p package
cargo test -p vm
python3 scripts/workflow/check_cpu_specific_arch_boundary.py
RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh
make workflow-gate
```

It also includes explicit behavior-regression and canonical package checks for
every registered family fixture. This phase does not require FS-UAE because it
changes no native production code; new 680x0 Level D work begins at Item 14.

## Mandatory Package-First Execution Boundary

Items 1-13 must finish before any native 680x0 implementation item starts. Their
purpose is not to copy the Rust handlers into another Rust abstraction. They
must compile the existing family-owned semantic authority into reusable,
versioned package records/programs and execute those serialized bytes through
the architecture-neutral Rust package runtime.

The package-first completion gate must fail closed unless all of the following
are simultaneously true:

1. every supported 680x0 profile and positive/negative corpus case executes
   from freshly serialized package bytes with Rust-identical bytes,
   diagnostics, widths, state transitions, and pass behavior;
2. a runtime callback audit records zero post-construction calls into m68k
   parsing, selection, legality, directive, or encoding handlers;
3. package behavior is independent of registry order and succeeds for exact
   Rust-built 680x0-only and combined packages while the exact Rust-built
   65x02-only package remains unchanged and green;
4. malformed/unsupported package operations fail in the generic package codec
   or runtime without falling back to family code;
5. all registered family/package fixtures pass compatibility and behavior
   regression gates, and each new reusable operation has cross-family evidence
   when another registered family can exercise the same semantic concept;
6. a complete operation-by-family applicability matrix accounts for every new
   operation and every registered family as adopted or inapplicable with a
   concrete reason; every applicable family uses the common improved path and
   its complete Rust behavior suite passes on the resulting package;
7. `scripts/workflow/run_m68k_package_runtime_completion.sh` emits a clean-tree
   receipt consumed as a hard prerequisite by Item 14 and every later item;
8. `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh` passes from
   the workspace root, including its unfiltered `cargo test --locked`, with all
   workspace crates and supported-family suites present and no new ignored,
   skipped, filtered, excluded, or waived test.

Only package construction may depend on the Rust m68k family implementation
after this gate. Rust assembly and native assembly must then be two consumers of
the same unmodified serialized semantic package.

## Version Impact

- Affected component(s): `.opcpu` package schema and builder, portable VM
  selection/encoding, native `tkpkg`/`opasm`, native CLI target-state and
  artifact paths, 65x02 package compiler/regressions, all-family package
  compatibility gates, FS-UAE parity harness, 680x0 corpus accounting, native docs
- Impact class: major
- Versioning decision: Item 1 retains container version 1 and adds optional
  `SEMV`, whose payload carries its own exact opcode version. Packages without
  `SEMV` retain their canonical bytes; unknown semantic opcode versions and
  malformed programs fail closed with an `SEMV` package-format diagnostic.
- Item 2 retains container version 1 and adds optional `VALP`, whose payload
  carries its own exact opcode version. Packages without `VALP` retain their
  canonical bytes; unknown value opcode versions and malformed value programs
  fail closed. The checked-in `.opasm` package fixtures are regenerated exact
  Rust-builder output and are not a native-only schema or transformation.
- Item 3 retains container version 1 and adds optional `OPRD`, whose payload
  carries its own exact record-program version. Packages without `OPRD` retain
  their canonical bytes; unknown record-program versions and malformed records
  fail closed. The checked-in `.opasm` package fixtures are regenerated exact
  Rust-builder output and remain direct inputs for the later native work.
- Item 4 retains container version 1 and extends optional `OPRD` with record
  schema version 2. Version 1 base records remain accepted unchanged; version 2
  adds only neutral nested-address, pair, range, list, and field structures.
  Unsupported versions and malformed structures fail closed, and the refreshed
  `.opasm` fixtures remain exact Rust-builder output.
- Item 5 retains container version 1 and extends optional `OPRD` with record
  schema version 3. Versions 1 and 2 remain accepted unchanged; version 3 adds
  only a neutral numeric format tag over nested records. Family- and CPU-scoped
  package programs own every FPU/AMMX meaning, and the refreshed `.opasm`
  fixtures remain exact Rust-builder output.
- Item 6 retains container version 1 and adds optional `SLCT`, whose payload
  carries its own exact opcode version. Packages without `SLCT` retain their
  canonical bytes; malformed and unsupported selector programs fail closed.
  Neutral exact maps, qualified-suffix rewrites, priority, and CPU allow lists
  keep aliases, ordering, applicability, and diagnostic choice in package data.
  The refreshed focused native package is exact Rust-builder output and remains
  within the unchanged 8,192-byte native allocation.
- Item 7 retains container version 1 and adds optional `STVM`, whose payload
  carries its own exact opcode version, plus lossless `CALS` compaction for CPU
  aliases whose family and default-dialect fields are exactly reconstructible.
  Richer aliases remain complete in `CPUS`; legacy packages without `STVM`
  retain canonical bytes, and malformed or unsupported state programs fail
  closed. The refreshed focused native package remains exact Rust-builder
  output at 8,122 bytes within the unchanged 8,192-byte native allocation.
- Item 8 retains container version 1 and adds independently versioned `SEMV`
  v2 programs for CPU-neutral literal, constrained-scalar, fixed-field, width,
  and endian emission. `SEMV` v1 remains accepted unchanged; malformed v2
  programs and missing or out-of-range inputs fail closed. The refreshed
  focused package remains exact Rust-builder output at 8,187 bytes within the
  unchanged 8,192-byte native allocation.
- Owned contract: the 68020/AmigaOS executable exposes the complete
  Rust-supported 680x0 assembler surface and can rebuild itself
- Rationale: CPU discovery is already present in package metadata, but complete
  m68k operand selection and encoding remain Rust-host callbacks and therefore
  are not portable to the native runtime

## Work Items

- [x] Item 1: add the versioned portable semantic-program substrate with a Rust-only fixed-opcode vertical slice
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-003`, `N68X0-004`, `N68X0-014`, `N68X0-015`.
  - Expected files: `crates/opforge-package/src/package.rs`, `crates/opforge-package/src/package/codec/**`, architecture-neutral Rust package runtime/builder modules, focused tests, one package-phase slice record; no `native/**` production file.
  - Full quality gates: Package Phase Quality Gate; focused package round-trip/canonicalization/malformed-opcode tests; fixed-opcode serialized-package execution; cross-family proof for every reusable primitive.
  - Plan-compliance review evidence: `PASS` — the independent `plan-compliance-reviewer` confirmed a focused, versioned, CPU-neutral `SEMV` substrate; legacy-byte compatibility; fail-closed malformed/unsupported programs; serialized-only fixed big-endian m68020 emission; cross-family reuse; no native production change; and passing focused, Rust, architecture-boundary, and workflow gates.
  - Commit outcome: one commit that serializes, reloads, selects, and emits a package-owned fixed big-endian opcode through the Rust package runtime.
  - Definition of done: freshly serialized bytes are the only execution input; compatible old packages remain supported or fail with the explicitly versioned diagnostic; malformed programs fail closed; no m68k callback executes after construction.

- [x] Item 2: materialize scalar, literal, and expression-value operand programs
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-003`-`N68X0-005`, `N68X0-014`, `N68X0-015`.
  - Expected files: neutral scalar/value package schema and Rust interpreter, family-owned compilation adapter, focused tests, one package-phase slice record; no `native/**` production file.
  - Full quality gates: Package Phase Quality Gate; signed/unsigned bounds, literal/expression, malformed-value, round-trip, and applicable cross-family value tests.
  - Plan-compliance review evidence: `PASS` — the independent `plan-compliance-reviewer` confirmed that `VALP` is optional, independently versioned, scoped, and CPU-neutral; m68k and MOS rules remain family-owned package builders; canonical round-trip, legacy byte identity, malformed/unsupported fail-closed behavior, signed/unsigned/range/normalization/literal/expression behavior, cross-family reuse, and serialized-only execution are proven; refreshed `.opasm` files are exact Rust-built artifacts; no native implementation changed; and the focused, full Rust, architecture-boundary, and workflow gates pass.
  - Commit outcome: one commit replacing runtime scalar/value callbacks with serialized neutral programs.
  - Definition of done: scalar and expression values match the family oracle with zero post-construction family parser calls.

- [x] Item 3: materialize register and base effective-address operand records
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-003`-`N68X0-005`, `N68X0-014`, `N68X0-015`.
  - Expected files: neutral register/address-component records, family compiler adapter, Rust interpreter tests, one package-phase slice record; no `native/**` production file.
  - Full quality gates: Package Phase Quality Gate; register, indirect, increment/decrement, displacement, indexed, absolute, PC-relative, immediate, malformed-record, and applicable cross-family addressing tests.
  - Plan-compliance review evidence: `PASS` — the independent `plan-compliance-reviewer` confirmed the optional, independently versioned, CPU-neutral `OPRD` substrate; family-owned register spelling adapters; serialized-only reconstruction of direct data/address registers, indirect update modes, displacement, indexed, absolute, PC-relative, and immediate records; cross-family reuse; typed malformed/missing-input failures; legacy-byte compatibility; exact Rust-built fixture refresh; and no native production change. The reviewer specifically rechecked and closed the direct-address-register coverage finding.
  - Commit outcome: one commit serializing all base effective-address components.
  - Definition of done: base addressing is reconstructed from package records without runtime family parsing or register-name logic in the generic VM.

- [x] Item 4: materialize full-extension, list, pair, and bit-field operand records
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-003`-`N68X0-006`, `N68X0-014`, `N68X0-015`.
  - Expected files: neutral nested-address/list/pair/range records, family compiler adapter, Rust tests, one package-phase slice record; no `native/**` production file.
  - Full quality gates: Package Phase Quality Gate; 68020 extension, list/pair, bit-field, bounds, malformed-record, round-trip, and applicable cross-family structured-operand tests.
  - Plan-compliance review evidence: `PASS` — the independent `plan-compliance-reviewer` confirmed CPU-neutral, v1-compatible `OPRD` v2 structures; family-owned full-extension and bit-field combinations and bounds; serialized-only reconstruction of indexed/index-suppressed nested addresses, direct/indirect pairs, ranges, nonempty lists, and all register/value field-selector combinations; fail-closed malformed/fuzz behavior and typed missing inputs; exact Rust-built fixtures; and no native production change. The reviewer rechecked and closed the omitted-shape, empty-list, and two-sided-bound findings.
  - Commit outcome: one commit eliminating runtime callbacks for structured integer operands.
  - Definition of done: all owned full-extension/list/pair/bit-field shapes match the family oracle from serialized records.

- [x] Item 5: materialize FPU and AMMX structured operand records
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-003`-`N68X0-006`, `N68X0-014`, `N68X0-015`.
  - Expected files: neutral format/list/group/nested-record operations, family compiler adapters, Rust tests, one package-phase slice record; no `native/**` production file.
  - Full quality gates: Package Phase Quality Gate; FPU format/list, VEA/pair/group/texture, malformed-shape, and round-trip tests; reviewed decomposition for concepts without cross-family applicability.
  - Plan-compliance review evidence: `PASS` — the independent `plan-compliance-reviewer` confirmed that `OPRD` v3 adds only a CPU-neutral numeric-tagged composite while retaining v1/v2 decoding; generic package/VM paths contain no FPU, AMMX, VEA, texture, or target-register semantics; family/CPU adapters own all FPU formats and m68080 register, VEA, pair, group, and texture mappings; serialized-only tests cover the complete requested matrices, CPU-scope rejection, malformed/fuzz/round-trip behavior, and typed missing records; exact Rust-built fixtures were refreshed; no native production implementation changed; and all required gates pass.
  - Commit outcome: one commit eliminating the remaining complex-operand runtime callbacks.
  - Definition of done: all FPU/AMMX operand shapes match the family oracle while generic operations remain CPU-neutral.

- [x] Item 6: materialize selector ordering, aliases, and selected diagnostic programs
  - Source requirement or finding IDs: `N68X0-001`-`N68X0-004`, `N68X0-014`, `N68X0-015`.
  - Expected files: neutral selector/alias/diagnostic records, family/dialect compiler adapter, Rust tests, one package-phase slice record; no `native/**` production file.
  - Full quality gates: Package Phase Quality Gate; selector order, aliases, ambiguity, missing/duplicate target, selected diagnostic, malformed-program, and cross-family selector tests.
  - Plan-compliance review evidence: `PASS` — the independent `plan-compliance-reviewer` confirmed optional, independently versioned, canonical, CPU-neutral `SLCT`; dialect → CPU → family precedence and deterministic priority; explicit duplicate/ambiguity failures; canonical CPU allow-list applicability; exact qualified-suffix rewrites and declared diagnostic selection; fail-closed malformed/unsupported programs; serialized-only alias, isolation, missing, diagnostic, and cross-family 65C02 reuse coverage; the boundary excluding later legality/encoding work; exact fixture-only native changes; and the 8,157/8,192-byte focused native package proof.
  - Commit outcome: one commit moving selection ordering and owned diagnostic choice into package programs.
  - Definition of done: selection and its diagnostics match the oracle without post-construction family selector callbacks.

- [x] Item 7: materialize capability legality and target-state programs
  - Source requirement or finding IDs: `N68X0-001`-`N68X0-006`, `N68X0-014`, `N68X0-015`.
  - Expected files: neutral capability/state records, family/CPU compiler adapters, Rust tests, one package-phase slice record; no `native/**` production file.
  - Full quality gates: Package Phase Quality Gate; CPU/FPU/Apollo pairing, state transition/reset, illegal combination, malformed-program, and cross-family capability tests.
  - Plan-compliance review evidence: `PASS` — after rejecting an initial lossy alias-compaction revision, the independent `plan-compliance-reviewer` confirmed versioned, CPU-neutral `STVM`; lossless `CALS`/`CPUS` alias handling; exhaustive six-profile × five-FPU-target legality plus Apollo rejection on every non-68080 profile; transactional transitions, reset/default behavior, serialized-only execution, cross-family MOS reuse, legacy-byte compatibility, fail-closed malformed/unsupported programs, exact 8,122/8,192 fixture capacity, no native production-source change, explicit assembler-callback cutover deferral to Item 13, and clean focused, full Rust all-family, architecture-boundary, and workflow gates.
  - Commit outcome: one commit moving runtime legality and directive-state decisions into package programs.
  - Definition of done: all six profiles make identical legality/state decisions without family legality or directive callbacks.

- [x] Item 8: materialize fixed field, endian, and value-validation encoding programs
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-003`-`N68X0-006`, `N68X0-014`, `N68X0-015`.
  - Expected files: neutral field/value/emission operations, family encoder compiler adapter, Rust tests, one package-phase slice record; no `native/**` production file.
  - Full quality gates: Package Phase Quality Gate; field insertion, endian, immediate/displacement, overflow, malformed-program, canonical package, and cross-family emission tests.
  - Plan-compliance review evidence: `PASS` — after requiring explicit destruction of compiler state plus literal and typed missing-input proof, the independent `plan-compliance-reviewer` confirmed CPU-neutral, independently versioned and v1-compatible `SEMV` v2; family-owned m68k field meaning; live Rust-oracle equality; serialized-only literal/scalar/fixed-field execution; big-endian signed displacement and bounded field insertion; cross-family MOS little-endian reuse; typed missing-input/overflow failures; malformed/fuzz coverage; exact 8,187/8,192 fixture capacity; no native production-source change; later structured/fixup/cutover work excluded; and clean focused, full Rust all-family, architecture-boundary, and workflow gates.
  - Commit outcome: one commit compiling directly resolvable encodings into portable programs.
  - Definition of done: fixed instruction/extension bytes match the oracle with zero runtime encoder callbacks.

- [ ] Item 9: materialize structured extension encoding programs
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-003`-`N68X0-006`, `N68X0-014`, `N68X0-015`.
  - Expected files: neutral list/pair/range/nested-extension encoding operations, family compiler adapter, Rust tests, one package-phase slice record; no `native/**` production file.
  - Full quality gates: Package Phase Quality Gate; list/pair/bit-field/FPU/AMMX extension, overflow, malformed-program, and applicable cross-family structured-emission tests.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for one structured-extension construction invariant.
  - Commit outcome: one commit compiling complex extension emission into neutral programs.
  - Definition of done: structured extensions match the oracle without runtime family encoder dispatch.

- [ ] Item 10: materialize PC-relative and deferred-fixup programs
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-003`-`N68X0-006`, `N68X0-014`, `N68X0-015`.
  - Expected files: neutral fixup/runtime-context operations, family fixup compiler adapter, Rust tests, one package-phase slice record; no `native/**` production file.
  - Full quality gates: Package Phase Quality Gate; unresolved symbol, PC-relative base, relocation, forward/backward reference, malformed-program, and cross-family fixup tests.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for fixup projection/evaluation only.
  - Commit outcome: one commit expressing deferred 680x0 values through neutral package fixups.
  - Definition of done: fixups and relocations match the oracle without a runtime family fixup callback.

- [ ] Item 11: materialize branch sizing and multi-pass convergence programs
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-003`-`N68X0-006`, `N68X0-014`, `N68X0-015`.
  - Expected files: neutral candidate-width/stability operations, family branch compiler adapter, Rust pass tests, one package-phase slice record; no `native/**` production file.
  - Full quality gates: Package Phase Quality Gate; widening, explicit size, convergence, overflow, deterministic failure, and applicable cross-family branch tests.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for the single branch-width convergence invariant.
  - Commit outcome: one commit moving branch sizing policy into package candidates/programs.
  - Definition of done: packaged execution converges to Rust-identical widths and bytes with no runtime family branch callback.

- [ ] Item 12: adopt every applicable neutral VM/package improvement across all existing CPU families
  - Source requirement or finding IDs: `N68X0-003`, `N68X0-013`, `N68X0-015`, `N68X0-016`.
  - Expected files: package compiler definitions for every registered family with an applicable migration, operation-by-family applicability matrix, focused per-family Rust tests, one package-phase slice record; no `native/**` production file.
  - Full quality gates: Package Phase Quality Gate; complete applicability matrix covering Items 1-11 and every registered family; each family's complete positive/negative, alias, addressing, selection, diagnostic, serialization, and registry-composition tests; canonical compatibility evidence; unfiltered root `cargo test --locked`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` only if every operation/family cell is accounted for, every applicable improvement uses the common path, every inapplicable cell has a concrete family-semantic reason, and any non-mechanical family work was split into its own prior reviewed item.
  - Commit outcome: one mechanical commit bringing every existing Rust-built family package onto every applicable improved common facility.
  - Definition of done: all registered families consume all applicable neutral improvements through their updated serialized packages, their complete Rust suites pass without behavior regression, and no family remains on a legacy-only runtime fork; the 65x02 package also remains ready for the retained native proof.

- [ ] Item 13: cut the Rust assembler over to complete package-only 680x0 execution and publish the package-first completion gate
  - Source requirement or finding IDs: `N68X0-001`-`N68X0-007`, `N68X0-013`-`N68X0-017`.
  - Expected files: Rust assembler/package adapter, all six 680x0 package builders, registry composition tests, callback guard, `scripts/workflow/run_m68k_package_runtime_completion.sh`, corpus accounting, focused tests, one package-phase slice record; no `native/**` production file.
  - Full quality gates: Package Phase Quality Gate; Item 12 all-family applicability matrix and complete per-family suites; full 680x0 positive/negative corpus through freshly serialized packages; single/combined package and registry-order tests; callback-audit zero assertion; package size report; `scripts/workflow/run_m68k_package_runtime_completion.sh`; final clean-tree `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh` whose unfiltered `cargo test --locked` covers every workspace crate and supported family.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` only after the complete callback audit, exact serialized inputs, corpus accounting, composition matrix, clean-tree receipt, workspace-membership diff, ignored/skipped-test diff, and complete Rust quality-gate transcript prove that no crate or supported-family test coverage was reduced.
  - Commit outcome: one commit making package-only execution the normal Rust 680x0 path and publishing the native-activation receipt.
  - Definition of done: the Mandatory Package-First Execution Boundary passes for all six profiles; direct family runtime paths are removed or test-oracle-only; exact single-family and combined packages are ready for native consumption.

- [ ] Item 14: implement the versioned portable selector/encoder substrate in the native generic runtime and prove fixed-opcode execution
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-003`, `N68X0-004`, `N68X0-008`, `N68X0-013`-`N68X0-016`.
  - Expected files: architecture-neutral native `tkpkg` package/selection/encoding modules, focused tests, one native parity slice record; no family package-definition change.
  - Full quality gates: Shared Full Quality Gate; Item 13 completion receipt; focused native exact-input-digest, package-version, and malformed-opcode tests; Level D `external_fs_uae_native_m68k_fixed_opcode_package_parity` using the unmodified Rust-builder byte vector.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for native implementation of the already-frozen generic substrate and confirms the staged package digest matches Item 13 and the shared 65x02 completion proof remains green.
  - Commit outcome: one commit making native `tkpkg` execute the package-owned fixed-opcode program without a family-specific branch.
  - Definition of done: real native execution consumes Item 13's exact serialized bytes and matches packaged Rust output; the current Rust-built 65x02 package remains fully native-executable; no native package transformation or m68k-owned identifier exists in generic runtime control flow.

- [ ] Item 15: implement neutral scalar/register and big-endian field operations in the native package runtime
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-003`-`N68X0-005`, `N68X0-014`-`N68X0-016`.
  - Expected files: native `tkpkg_operand_runtime.asm` and `tkpkg_encode_service.asm`, native/Rust parity tests, one slice record; Item 13 package bytes are frozen inputs.
  - Full quality gates: Shared Full Quality Gate; focused bounds/endianness/register-field tests; Level D `external_fs_uae_native_m68000_scalar_register_encoding_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for scalar, immediate, register-field, and big-endian word/long operations only.
  - Commit outcome: one commit enabling package-owned `MOVEQ`, immediate, and register-only forms.
  - Definition of done: signed/unsigned bounds, field insertion, and big-endian words/longs match Rust for positive and negative cases without native register-name logic.

- [ ] Item 16: implement the frozen base effective-address record in the native generic decoder
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-003`-`N68X0-005`, `N68X0-014`-`N68X0-016`.
  - Expected files: native generic package operand interpreter, native/Rust parity tests, one slice record; no package schema or family compiler change.
  - Full quality gates: Shared Full Quality Gate; focused Level A/B corpus for `Dn`, `An`, indirect, postincrement, predecrement, displacement, indexed, absolute, PC-relative, and immediate shapes; Level D `external_fs_uae_native_m68000_effective_address_record_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for the base-EA record and decoder only.
  - Commit outcome: one commit that transports base 68000 EA semantics as package data and decodes them architecture-neutrally on AmigaOS.
  - Definition of done: every base EA record round-trips, malformed records fail closed, and the native decoder contains no m68k-owned spellings.

- [ ] Item 17: prove native package execution for base 68000 movement and control flow
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-002`, `N68X0-004`, `N68X0-005`, `N68X0-014`-`N68X0-016`.
  - Expected files: native generic interpreter only if a frozen neutral operation is missing, parity tests, one slice record; no package/family-definition change.
  - Full quality gates: Shared Full Quality Gate; focused `68000_basic_moves`, `68000_addressing_matrix`, `68000_control_addressing`, and `68000_qualified_jsr` parity; Level D `external_fs_uae_native_m68000_move_control_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for movement and control-flow forms only.
  - Commit outcome: one commit covering `MOVE/MOVEA/MOVEQ/LEA/PEA/JMP/JSR` and package-selected fixed control forms.
  - Definition of done: complete owned fixtures and their negative EA cases match live Rust bytes/diagnostics.

- [ ] Item 18: prove native package execution for base 68000 ALU, bit, and shift/rotate behavior
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-002`, `N68X0-004`, `N68X0-005`, `N68X0-014`-`N68X0-016`.
  - Expected files: generic native VM extensions only for frozen neutral operations, parity tests, one slice record; no package/family-definition change.
  - Full quality gates: Shared Full Quality Gate; focused arithmetic/bit/shift fixture parity; Level D `external_fs_uae_native_m68000_alu_bit_shift_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for the named base instruction families only.
  - Commit outcome: one commit covering the base ALU/compare/quick/bit/shift/rotate surface.
  - Definition of done: all sizes, direction variants, immediate forms, and illegal destinations in the owned fixtures match Rust.

- [ ] Item 19: prove native package execution for base 68000 branches and multi-pass stability
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-005`, `N68X0-008`, `N68X0-009`, `N68X0-014`-`N68X0-016`.
  - Expected files: native generic runtime-context/selection/fixup owners, assembler pass parity tests, one slice record; frozen package operations remain unchanged.
  - Full quality gates: Shared Full Quality Gate; focused unresolved/widening, explicit-size, displacement, `Bcc/BSR/DBcc/Scc` tests; Level D `external_fs_uae_native_m68000_branch_stability_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for one branch-sizing/stability invariant.
  - Commit outcome: one commit making branch selection stable across native passes.
  - Definition of done: final displacements and widths match Rust; a moved failure or prefix-only pass cannot count as closure.

- [ ] Item 20: prove native package execution for the remaining base 68000 surface
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-002`, `N68X0-004`, `N68X0-014`-`N68X0-016`.
  - Expected files: native generic decoder support for frozen list/pair records only if needed, parity tests, one slice record; no package/family-definition change.
  - Full quality gates: Shared Full Quality Gate; focused `MOVEM`, `MOVEP`, system-register, BCD, exchange, extend, multiply/divide/check fixtures; Level D `external_fs_uae_native_m68000_remaining_base_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for the remaining base-68000 corpus only.
  - Commit outcome: one commit completing the `m68000` positive surface.
  - Definition of done: every applicable `68000_*` source/reference case is green and every base negative case returns the Rust-equivalent diagnostic.

- [ ] Item 21: prove native package execution for the 68010 delta and legality matrix
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-002`, `N68X0-006`, `N68X0-014`-`N68X0-016`; `AC-M68KLINEAGE-001`-`003`.
  - Expected files: native package selection/state interpreter only if a neutral operation is incomplete, parity tests, one slice record; frozen 68010 package programs remain unchanged.
  - Full quality gates: Shared Full Quality Gate; focused aliases and `BKPT/MOVEC/MOVES/RTD/MOVE CCR` tests; Level D `external_fs_uae_native_m68010_delta_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for the 68010 delta and earlier-CPU rejections only.
  - Commit outcome: one commit completing the `m68010` profile.
  - Definition of done: `68010_delta` matches Rust and the same later-only forms fail identically on `m68000`.

- [ ] Item 22: prove native package execution for 68020 full-extension addressing
  - Source requirement or finding IDs: `N68X0-001`-`N68X0-005`, `N68X0-014`-`N68X0-016`; `AC-M68KLINEAGE-004`, `AC-M68KLINEAGE-005`.
  - Expected files: native generic decoder for the frozen complex-operand records, parity tests, one slice record; no package/family-definition change.
  - Full quality gates: Shared Full Quality Gate; direct/preindexed/postindexed/base-suppressed/index-suppressed and alias tests; Level D `external_fs_uae_native_m68020_full_extension_addressing_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for full-extension addressing only.
  - Commit outcome: one commit enabling package-owned 68020+ full-extension EAs.
  - Definition of done: the complete fixture matches Rust on 68020/030/040 and rejects the forms on 68000/010.

- [ ] Item 23: prove native package execution for the first 68020 later-family group
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-002`, `N68X0-004`, `N68X0-014`-`N68X0-016`; `AC-M68KLINEAGE-006`.
  - Expected files: generic native implementations of already-frozen operations only as required, parity tests, one slice record; no package/family-definition change.
  - Full quality gates: Shared Full Quality Gate; long branches, `LINK.L`, `EXTB.L`, long multiply/divide, `CAS/CAS2`, and `CHK2/CMP2`; Level D `external_fs_uae_native_m68020_later_integer_group_a_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for exactly the named group-A forms.
  - Commit outcome: one commit covering the first half of the Rust 68020 delta.
  - Definition of done: positive, pair/register legality, and earlier-CPU negative cases match Rust.

- [ ] Item 24: prove native package execution for the remaining 68020 and residual-risk group
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-002`, `N68X0-004`, `N68X0-014`-`N68X0-016`; `AC-M68KLINEAGE-006`; `AC-M68K-RESIDUAL-001`-`008`.
  - Expected files: native generic implementations of frozen bit-field/pair/list operations only if needed, parity tests, one slice record; no package/family-definition change.
  - Full quality gates: Shared Full Quality Gate; bit fields, `PACK/UNPK`, `TRAPcc`, `CALLM/RTM`, long-divide aliases, `MOVES` EA legality, and `CAS2` field legality; Level D `external_fs_uae_native_m68020_later_integer_group_b_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for exactly the named group-B and residual contracts.
  - Commit outcome: one commit completing the Rust-supported `m68020` integer profile.
  - Definition of done: `68020_later_families` and all residual-risk cases match Rust without widening the intentionally excluded PMMU surface.

- [ ] Item 25: implement frozen package-owned target-state operations in the native generic runtime
  - Source requirement or finding IDs: `N68X0-003`, `N68X0-004`, `N68X0-006`, `N68X0-014`-`N68X0-016`.
  - Expected files: architecture-neutral native directive/state adapter, parity tests, one slice record; frozen package runtime-state descriptors and family definitions remain unchanged.
  - Full quality gates: Shared Full Quality Gate; state reset/switch/illegal-pair tests; Level D `external_fs_uae_native_m68k_runtime_directive_state_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` only if generic native code interprets package state operations and contains no FPU/Apollo target table.
  - Commit outcome: one commit enabling package-owned target-state directives.
  - Definition of done: source and CLI CPU selection, `.fpu`, and `.apollo` transitions/reset behavior match Rust across multiple files and runs.

- [ ] Item 26: prove native package execution for m68030/m68040 integer and shipped MMU behavior
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-002`, `N68X0-006`, `N68X0-014`-`N68X0-016`; `AC-M68KLINEAGE-007`, `AC-M68KLINEAGE-008`; `AC-M68KMF-003`, `AC-M68KMF-004`, `AC-M68KMF-006`.
  - Expected files: native neutral capability/legality interpreter only if incomplete, parity tests, one slice record; frozen 68030/040 package definitions remain unchanged.
  - Full quality gates: Shared Full Quality Gate; carry-forward, `PFLUSH`, MMU `MOVEC`, `MOVE16`, and `CALLM/RTM/CAAR` restriction tests; Level D `external_fs_uae_native_m68030_m68040_integer_mmu_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for the shipped narrow MMU and 030/040 integer delta only.
  - Commit outcome: one commit completing Rust-supported 68030/040 non-FPU behavior.
  - Definition of done: owned positive fixtures and all explicit restriction references match Rust; unshipped PMMU forms remain deterministic failures.

- [ ] Item 27: prove native package execution for the 68881/68882 core surface
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-002`, `N68X0-006`, `N68X0-014`-`N68X0-016`; `AC-M68KMF-001`, `002`, `005`, `006`.
  - Expected files: generic native interpreter implementations for frozen format/list operations only if needed, parity tests, one slice record; no package/family-definition change.
  - Full quality gates: Shared Full Quality Gate; `68020_fpu_allmodes`, registers, core arithmetic, conditionals, save/restore; Level D `external_fs_uae_native_m68881_m68882_core_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for the external-FPU core surface only.
  - Commit outcome: one commit enabling the core external-FPU profile on 68020/030.
  - Definition of done: legal target pairs, formats, registers, lists, and illegal destinations match Rust for both 68881 and 68882.

- [ ] Item 28: prove native package execution for the remaining external-FPU catalog
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-002`, `N68X0-006`, `N68X0-014`-`N68X0-016`; `AC-M68KMF-005`, `006`.
  - Expected files: native parity tests and generic interpreter fixes only if a frozen neutral operation is incomplete, one slice record; no package/family-definition change.
  - Full quality gates: Shared Full Quality Gate; complete `68020_fpu_instruction_catalog`; Level D `external_fs_uae_native_m68881_m68882_extended_math_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for remaining external-FPU mnemonics only.
  - Commit outcome: one commit completing the Rust external-FPU catalog.
  - Definition of done: every catalog row and its applicable negative target case matches Rust.

- [ ] Item 29: prove native package execution for the integrated 68040 FPU surface
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-002`, `N68X0-006`, `N68X0-014`-`N68X0-016`; `AC-M68KMF-001`, `002`, `005`-`007`.
  - Expected files: native parity tests and neutral capability/encoder interpreter fixes only if needed, one slice record; frozen 68040 package programs remain unchanged.
  - Full quality gates: Shared Full Quality Gate; integrated core/conditional/save-restore and `FSIN`/external-target rejection tests; Level D `external_fs_uae_native_m68040_integrated_fpu_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for integrated-68040 FPU behavior only.
  - Commit outcome: one commit completing the Rust-supported m68040 FPU profile.
  - Definition of done: `68040_integrated_fpu` and all matching `.err` fixtures agree with Rust.

- [ ] Item 30: prove native package execution for the 68080 integer and control surface
  - Source requirement or finding IDs: `N68X0-001`-`N68X0-006`, `N68X0-014`-`N68X0-016`; `REQ-68080-FULL-001`-`007`, `009`-`011`.
  - Expected files: generic native interpreter implementations for frozen bank/pair/state records only if needed, parity tests, one slice record; no package/family-definition change.
  - Full quality gates: Shared Full Quality Gate; complete 68080 integer matrix and non-68080 rejections; Level D `external_fs_uae_native_m68080_integer_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for 68080 non-AMMX integer/control behavior only.
  - Commit outcome: one commit completing the Rust 68080 integer profile.
  - Definition of done: `68080_integer_addressing_matrix` and all owned alias/gating cases match Rust.

- [ ] Item 31: prove native package execution for the 68080 AMMX surface
  - Source requirement or finding IDs: `N68X0-001`-`N68X0-006`, `N68X0-014`-`N68X0-016`; `REQ-68080-FULL-001`, `004`, `005`, `007`-`011`.
  - Expected files: native generic interpreter implementations for frozen VEA/pair/group/texture records only if needed, parity tests, one slice record; no package/family-definition change.
  - Full quality gates: Shared Full Quality Gate; AMMX slice, addressing matrix, additional surface, and shape errors; Level D `external_fs_uae_native_m68080_ammx_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for AMMX only and verifies package ownership of mnemonic/register semantics.
  - Commit outcome: one commit completing the Rust AMMX surface.
  - Definition of done: all legal AMMX bytes, aliases, pair/alignment/group constraints, nested TEX shapes, and negative diagnostics match Rust.

- [ ] Item 32: prove native package execution for the 68080 FPU surface
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-002`, `N68X0-006`, `N68X0-014`-`N68X0-016`; `REQ-68080-FULL-002`, `003`, `006`-`010`.
  - Expected files: native parity tests and generic interpreter fixes only if a frozen neutral operation is incomplete, one slice record; frozen 68080 FPU package programs remain unchanged.
  - Full quality gates: Shared Full Quality Gate; complete 68080 FPU surface and illegal target pairings; Level D `external_fs_uae_native_m68080_fpu_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for 68080 FPU behavior only.
  - Commit outcome: one commit completing the sixth Rust target profile.
  - Definition of done: default/explicit/disabled FPU state and every owned 68080 FPU fixture match Rust.

- [ ] Item 33: prove unmodified Rust-built 680x0-only, 65x02-only, and combined package composition on native
  - Source requirement or finding IDs: `N68X0-003`, `N68X0-004`, `N68X0-006`, `N68X0-008`, `N68X0-009`, `N68X0-013`-`N68X0-016`.
  - Expected files: native package loader/selection tests, exhaustive corpus matrix, FS-UAE parity harness, package capacity measurements, one slice record; the exact Item 13 Rust-built composition artifacts are immutable test inputs.
  - Full quality gates: Shared Full Quality Gate; byte-identity/digest, TOC/order-independence, family-presence/absence, target-switch/reset, and malformed-package tests; existing complete native 65x02 completion wrapper; Level D `external_fs_uae_native_rust_package_composition_matrix_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for package composition and direct consumption only, confirms all three inputs came directly from the same Rust package-builder API, and finds no native-only package artifact or transformation step.
  - Commit outcome: one commit making native package discovery/selection composition-independent for exact Rust-produced family packages.
  - Definition of done: the native runtime consumes byte-for-byte unmodified Rust output in all three mandatory cases: (1) 680x0-only passes the complete applicable positive/negative inventory under `examples/motorola68000/**` and `examples/reference/motorola68000/**` and rejects unavailable 65x02 targets, (2) 65x02-only passes `scripts/workflow/run_native_existing_parity_completion.sh --verify` with the current Rust-built package and rejects unavailable 680x0 targets, and (3) combined runs the complete positive corpora for both families plus repeated `.cpu` switching in either registry order with Rust-identical bytes, state reset, and missing/duplicate-target diagnostics. The harness proves input digests and forbids conversion, extraction, reserialization, patching, semantic sidecars, case caps, representative probes, or reviewed exclusions for supported behavior.

- [ ] Item 34: make the complete Motorola 68000 reference corpus a mandatory native parity matrix
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-002`, `N68X0-007`-`N68X0-009`.
  - Expected files: `crates/opforge-asm/src/native_reference_parity.rs`, `fs_uae_smoke.rs`, `tests/native_fs_uae_parity.rs`, examples/references only through controlled refresh, completion wrapper, one slice record.
  - Full quality gates: Shared Full Quality Gate; completeness/accounting guards; `cargo test -p asm examples_match_reference_outputs -- --nocapture`; fail-closed Level D `external_fs_uae_native_motorola68000_complete_reference_parity` with no case cap.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for coverage/gating only; any discovered native fix must be inserted as a separate one-invariant work item.
  - Commit outcome: one commit promoting the complete applicable 680x0 corpus into a required native completion gate.
  - Definition of done: every top-level 680x0 source and every applicable `.srec/.lst/.err` artifact is accounted for; all cases run even after a failure; no reviewed exclusion represents missing behavior owned by this plan.

- [ ] Item 35: implement native Hunk headers, ordered CODE/DATA/BSS segments, memory attributes, and padding
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-009`, `N68X0-010`; Hunk v0.2/v0.3 executable contracts.
  - Expected files: native output-artifact/Hunk owner, CLI/source-output routing, section/layout projection, focused Hunk parser tests, one slice record.
  - Full quality gates: Shared Full Quality Gate; byte-level header/table/segment/padding/order/memory-bit tests; Level D `external_fs_uae_native_hunk_segment_surface_parity` including loader execution.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for Hunk container/segment rendering only, with no relocation or CPU-semantic work.
  - Commit outcome: one commit writing Rust-equivalent executable Hunk structure for flat and explicitly selected CODE/DATA/BSS sections.
  - Definition of done: HUNK_HEADER/CODE/DATA/BSS/END records, longword padding, empty-section omission, declared order, CHIP/FAST/ANY attributes, first-segment legality, CLI `--hunk`, and source `format=hunk` behavior match Rust.

- [ ] Item 36: complete native Hunk fixup, relocation, implicit-code, and diagnostic parity
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-005`, `N68X0-009`, `N68X0-010`; Hunk v0.2/v0.3 executable contracts.
  - Expected files: native generic fixup/session records, Hunk relocation writer, implicit-section/output routing, frozen package fixup metadata consumed as read-only input, focused tests, one slice record; no package/family-definition change.
  - Full quality gates: Shared Full Quality Gate; complete Rust Hunk compatibility matrix and unsupported-shape diagnostics; Level D `external_fs_uae_native_hunk_complete_executable_parity` including all runnable AmigaOS examples.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for the remaining Rust-supported executable-Hunk surface and verifies that object/library/debug/symbol/overlay hunks remain out of scope.
  - Commit outcome: one commit completing native HUNK_RELOC32/fixup and implicit executable-Hunk behavior.
  - Definition of done: every Hunk behavior accepted by the Rust executable contract produces identical bytes and loads/runs when applicable; every Rust-rejected relocation/notation form fails with the equivalent deterministic diagnostic.

- [ ] Item 37: implement complete native Motorola S-record output and multi-output routing
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-005`, `N68X0-009`, `N68X0-010`.
  - Expected files: native S-record artifact owner, CLI `-s/--srec` and `-g/--go` parsing/state, source `format=srec` routing, section/image projection, tests, one slice record.
  - Full quality gates: Shared Full Quality Gate; S0/data/count/S7-S9, address-width, checksum, line-length, sparse/section-order, start-address, simultaneous-output, and malformed-request tests; Level D `external_fs_uae_native_srec_complete_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for S-record output and multi-output routing only.
  - Commit outcome: one commit completing the Rust-supported S-record surface on AmigaOS.
  - Definition of done: CLI and source-declared S-record outputs, automatic address record selection, checksums/counts, `-g` termination address, sparse/selected sections, and deterministic failures match Rust byte-for-byte; requesting Hunk, S-record, and listing together writes all three without state leakage.

- [ ] Item 38: remove full-product resource blockers and prove simultaneous Hunk/S-record/listing generation
  - Source requirement or finding IDs: `N68X0-005`, `N68X0-009`, `N68X0-010`.
  - Expected files: native package/source/session/fixup/output allocation and bounds owners, CLI artifact orchestration, focused tests, one slice record.
  - Full quality gates: Shared Full Quality Gate; exact package/source/statement/label/fixup/image/output budget tests; Hunk structural validator and S-record parser; Level D `external_fs_uae_native_opforge_full_product_artifact_parity`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for measured resource and orchestration blockers only, with no new CPU semantics.
  - Commit outcome: one commit allowing the complete source graph and 680x0 package to emit all self-host artifacts in one invocation.
  - Definition of done: no fixed capacity rejects the current full product/package; overflow still fails deterministically; the native CLI produces Rust-identical Hunk and S-record artifacts plus the normalized-identical listing without a host postprocessor.

- [ ] Item 39: add the canonical same-command self-host harness and command-normalization guard
  - Source requirement or finding IDs: `N68X0-009`-`N68X0-016`.
  - Expected files: FS-UAE native harness, self-host command manifest/helper, deterministic command-normalization tests, completion wrapper, one slice record.
  - Full quality gates: Shared Full Quality Gate; Level A tests reject flag/value/order differences and allow only reviewed path translation; Level B staging/cleanup tests; preparatory Level D `external_fs_uae_native_opforge_self_host_generation_one_parity` comparing Hunk, S-record, and listing outputs.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for harness/guard behavior and confirms no evidence filename selects an oracle.
  - Commit outcome: one commit creating a fail-closed, ephemeral two-generation self-host harness.
  - Definition of done: the harness derives both commands from one logical argv vector, stages the exact product source/package tree, captures all three outputs in memory, and rejects every non-path difference.

- [ ] Item 40: run and promote the terminal two-generation native self-build proof
  - Source requirement or finding IDs: `N68X0-001`, `N68X0-009`-`N68X0-016`.
  - Expected files: self-host completion wrapper/gate registration, plan checkbox/evidence, documentation/release notes if version-impact evidence requires them, one final slice record.
  - Full quality gates: Shared Full Quality Gate; package-first completion from Item 13; native package-composition parity from Item 33; complete reference parity from Item 34; full Hunk parity from Items 35-36; full S-record parity from Item 37; product artifact parity from Item 38; final Level D `external_fs_uae_native_opforge_two_generation_self_host_parity`; `make quality-gate`; `make workflow-gate`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` only after reviewing the exact staged index, logical argv, path mapping, fresh protocols, both guest exits, in-memory comparisons of all outputs, and cleanup evidence.
  - Commit outcome: one commit promoting self-hosting to the mandatory native completion gate and recording plan completion.
  - Definition of done: the final test below passes exactly as written in the Self-Hosting Acceptance Contract; generation 1 and generation 2 are complete, runnable native `opforge` executables and emit Rust-identical Hunk, S-record, and listing artifacts from an unmodified Rust-built 680x0-only package; no reduced source tree, alternate flags, helper assembler, native package rewrite, stored oracle, stale artifact, or launcher-only success is accepted.

## Self-Hosting Acceptance Contract

Item 39 must store one canonical logical argv vector equivalent to:

```text
opforge
--cpu 68020
--opasm-package native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm
-l build/opforge.lst
--hunk build/opforge
-s build/opforge.srec
-M native/motorola68000/amigaos/opforge-cli
-M native/motorola68000/amigaos/tkpkg
-M native/motorola68000/amigaos/tkvm
-M native/motorola68000/amigaos/prvm
-M native/motorola68000/amigaos/exprvm
-M native/motorola68000/amigaos/opcore
-M native/motorola68000/amigaos/opasm
-M native/motorola68000/amigaos/debug
-I native/motorola68000/amigaos/debug
native/motorola68000/amigaos/main.asm
```

The exact vector may be amended before Item 39 only when the product's
canonical build genuinely changes; such an amendment requires renewed plan
quality approval. The executable is staged under the same basename `opforge` on
both hosts. The `--opasm-package` operand must resolve to the exact
Rust-builder-produced 680x0-only package whose digest is recorded in the actual
test case; staging may change its path notation but not one byte. The path
translator may rewrite only path-valued tokens, for
example `native/.../main.asm` to `Work:native/.../main.asm` and
`build/opforge` to `Work:build/opforge`. It may not add, remove, reorder, or
change any non-path token.

The terminal test must:

1. construct a fresh actual case containing the exact source bytes, the exact
   unmodified Rust-built 680x0-only package bytes and digest, the logical argv,
   and the in-memory Rust Hunk/S-record/listing oracle;
2. use the Rust CLI with the Unix rendering of that argv to build the
   generation-0 oracle and bootstrap native executable;
3. launch generation 0 under FS-UAE and use the AmigaDOS rendering of the same
   argv to build generation 1 from the complete staged native tree;
4. verify fresh start/done challenges, explicit zero guest exit, byte-for-byte
   Hunk and S-record equality, and approved deterministic listing equality;
5. launch the newly produced generation 1 executable and run the same AmigaDOS
   command again to build generation 2;
6. verify generation 2 with a new challenge, explicit zero exit, equality of
   Hunk, S-record, and listing output to the same-command Rust oracle and
   generation 1, and successful executable loading—not merely Hunk structure;
7. return required evidence in memory and remove every case input, output,
   marker, log, mounted tree, and derived artifact before the runner returns on
   pass, failure, timeout, crash, or unwind.

This is the final test in the plan. No later functional test or implementation
item may weaken or supersede it.

## Milestones

- [ ] Milestone 1: the Rust assembler executes all supported 680x0 behavior
  exclusively from CPU-neutral serialized package programs; every registered
  family has adopted every applicable improvement, remains green, and the
  package-first receipt is valid (Items 1-13).
- [ ] Milestone 2: the frozen portable substrate and base operand records are
  native-executable without family-specific native logic (Items 14-16).
- [ ] Milestone 3: complete native `m68000` and `m68010` package parity is green
  (Items 17-21).
- [ ] Milestone 4: complete native `m68020`, `m68030`, and `m68040` integer/MMU
  package parity is green (Items 22-26).
- [ ] Milestone 5: external and integrated pre-68080 native FPU package parity
  is green (Items 27-29).
- [ ] Milestone 6: complete native `m68080` integer, AMMX, and FPU package parity
  is green (Items 30-32).
- [ ] Milestone 7: complete native package-composition and reference parity,
  full executable-Hunk and S-record parity, and simultaneous full-product output
  are mandatory and green (Items 33-38).
- [ ] Milestone 8: generation-0 native `opforge` builds generation 1, and
  generation 1 builds generation 2 with the same logical command; all outputs
  satisfy the terminal proof contract (Items 39-40).

## Blocking Rules

- activation prerequisites must pass before Item 1 starts
- no commit before all focused commands and the applicable Package Phase or
  Shared Full Quality Gate pass
- `plan-compliance-reviewer` must return `PASS` before commit
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- Item 14 is blocked unless Item 13's final unfiltered root `cargo test --locked`
  passes the entire Rust workspace corpus with all supported families included
  and no new ignore, skip, filter, exclusion, default-member reduction, or waiver
- checkbox updates are mandatory bookkeeping
- any discovered divergence pauses the active item at the first divergent boundary
- architecture-specific behavior in generic native/Rust paths blocks the commit
- required Level D execution for Items 14-40 may not skip; missing FS-UAE
  configuration blocks native-phase completion
- the final self-host test may not substitute different flags, a reduced tree, helper entrypoints, or persisted evidence
- archive the completed plan and sidecar with `scripts/workflow/archive_completed_plan.sh`
