# libopforge FFI Full API Expansion Plan v0.1

## Metadata

- Source: user request on `2026-03-20`, `documentation/libopforge-developer-guide.md`, `documentation/libopforge-embedding-cookbook.md`, `crates/opforge-lib/src/lib.rs`, `crates/opforge-ffi/opforge.h`, and `dev-docs/NextSteps/libopforge_ffi_api_upgrade_spec_2026-03-17_rev1.md`
- Mode: migration
- Owner: active implementation agent for `feature/libopforge-lib`

## Objective

Expand the C FFI layer so it can represent the full stable public `libopforge`
Rust facade instead of only the current assembler-heavy subset. The finished
FFI should treat `libopforge` as the source of truth for host-facing concerns
and should let non-Rust hosts consume the same major workflows that the Rust
facade already supports:

- high-level assembly and prepared-session workflows
- diagnostics and lockstep reporting
- generic language-core services (`opcore`)
- processor-neutral routing (`processing`)
- registry and capability discovery
- formatter execution
- host I/O integration patterns that correspond to the stable Rust adapters

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- One work item is active at a time and each item must end in exactly one new
  commit.
- The stable public Rust facade in `crates/opforge-lib/src/lib.rs` is the
  compatibility source of truth. New FFI coverage should map that facade rather
  than exposing arbitrary lower-level crate internals.
- Scope is limited to the stable public `libopforge` module tree:
  `asm`, `asm::opasm`, `diagnostics`, `io`, `processing`, `registry`,
  `lockstep`, `formatter`, and `opcore`.
- If the current high-level FFI upgrade spec and the full-surface expansion plan
  disagree, the conflict must be resolved explicitly in docs before landing a
  breaking ABI redesign.
- Additive ABI growth is preferred until a deliberate breaking-change decision
  is approved.
- New coverage must come with ABI contract tests, release-ffi export coverage,
  and focused behavior tests for each newly exposed concern.

## Current Coverage Snapshot

The current FFI layer already covers meaningful parts of the facade, but it is
not close to full-surface parity.

| Stable facade area | Rust facade status | Current FFI status | Coverage summary |
|---|---|---|---|
| `asm` | high-level builders, one-shot runs, session/prepared workflows, reports | one-shot file/memory entrypoints, session handles, prepared session run/check, report accessors | partial |
| `diagnostics` | full run reports, diagnostics, fixits, source-line context helpers | diagnostic enumeration and fixits over `opforge_asm_report` | partial |
| `io` | `SourceProvider`, `OutputSink`, filesystem and memory adapters | memory source text entrypoints and output callbacks only | low |
| `processing` | editor routing, model-aware routing, neutral engine error contracts, traces | statement processing traces only; no editor-routing family | low |
| `registry` | registry construction, CPU resolution, transition scanning, capability reports | default registry + list-style lookup and per-CPU string views | partial |
| `lockstep` | execution-mode contracts plus structured lockstep reports | statement-processing lockstep traversal only | partial |
| `formatter` | stable formatter config, engine, output, diagnostics, summaries | no formatter API exported | none |
| `opcore` | tokenization, expressions, line/module-item parsing, preprocess, macros, portable contracts | tokenize, parse-expression AST walk, module-item processing | partial |
| `asm::opasm` | tokenization, parsing, processing, builder/processor helpers, portable variants | tokenize, parse, process entrypoints and trace/lockstep accessors | partial |

## Gap Summary

### Well-covered today

- High-level assembly/check entrypoints over files and in-memory source text.
- Owned session and prepared-session execution handles.
- Assembly diagnostics, related spans, notes, help, and fixits.
- Basic registry enumeration for CPUs, families, dialects, directives, and
  CPU-local mnemonic/register/runtime-directive strings.
- Statement-level tokenize/parse/process entrypoints for the current `opasm`
  slice.

### Missing or materially incomplete

1. Prepared-session metadata parity.
   Rust exposes `root_module_id()`, `cpu_name()`, `source_map()`, and
   `dependency_files()` on prepared values. The current FFI exposes prepared
   execution but not prepared metadata inspection.

2. Assembly-report metadata parity.
   Rust `AsmRunReport` exposes richer host data such as source lines,
   runtime-processing traces, and structured lockstep reports. The current FFI
   only exposes counts for high-level lockstep data and does not publish
   report-owned trace/report traversal objects.

3. `formatter` is completely absent.
   The stable Rust facade publishes `FormatterEngine`, `FormatterConfig`, and
   formatter reports, but the FFI layer has no corresponding types or entrypoints.

4. `processing` routing APIs are absent.
   Rust hosts can call `editor_route_line`, `route_module_item_line`,
   `editor_route_line_with_model`, and
   `editor_route_line_with_model_in_mode`. The FFI layer does not expose this
   processor-neutral routing boundary.

5. `registry` resolution/capability helpers are not mapped.
   The Rust facade exports `resolve_target_cpu`, `resolve_cpu_for_line`,
   `scan_cpu_transitions`, `default_cpu`, `capabilities_report`,
   `capabilities_report_json`, `cpusupport_report`, and
   `cpusupport_report_json`. The FFI currently exposes only basic enumeration.

6. `opcore` is only partially mapped.
   Missing areas include `editor_parse_line`, `parse_expression_tokens`, and the
   stable preprocess/macro interfaces or an explicit decision that those remain
   Rust-only.

7. `asm::opasm` portable and processor-helper paths are not mapped.
   The Rust facade includes `ProcessorBuilder`, `Processor`, and portable
   tokenize/parse/process result types, while the FFI currently exposes only the
   direct request functions.

8. `io` parity is weak.
   The Rust facade has stable `SourceProvider` and `OutputSink` abstractions plus
   memory/filesystem implementations. The FFI currently supports only one
   special-case memory source path and output-file callbacks, not a reusable
   source/output provider contract.

## Expansion Principles

1. Map the stable facade, not internal crate topology.
2. Keep ownership rules explicit and simple for C callers.
3. Reuse opaque handles and accessor families instead of copying large native
   Rust structs into the ABI.
4. Prefer result/report patterns that can scale across all facade areas.
5. Add release-ffi export coverage for every new public symbol family.
6. Avoid a single giant landing. Expand one concern boundary at a time.

## Work Items

- [ ] Item 1: Freeze the public-surface inventory and add an FFI coverage manifest
  - Source requirement or finding IDs: user request on `2026-03-20`; stable facade modules documented in `documentation/libopforge-developer-guide.md`
  - Expected files: `documentation/libopforge-ffi-full-api-expansion-plan-v0_1.md`; `documentation/libopforge-developer-guide.md`; `crates/opforge-ffi/tests/`; optionally a new generated or handwritten surface-manifest test input under `crates/opforge-ffi/tests/`
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace --all-targets -- -D warnings`; `cargo audit`; `make test`
  - Plan-compliance review evidence: `plan-compliance-reviewer` PASS citing the finalized surface inventory, the chosen parity target, and the validation commands
  - Commit outcome: planned: one commit that makes the stable Rust surface and current FFI coverage mechanically reviewable before broader implementation starts
  - Definition of done: the repo contains one authoritative coverage inventory that names each stable `libopforge` concern and whether the FFI target is full, partial, deferred, or intentionally Rust-only

- [ ] Item 2: Add prepared-session and assembly-report metadata parity
  - Source requirement or finding IDs: missing prepared metadata and report metadata parity from this plan
  - Expected files: `crates/opforge-ffi/src/lib.rs`; `crates/opforge-ffi/opforge.h`; `crates/opforge-ffi/tests/abi_contract.rs`; `crates/opforge-ffi/tests/`; `documentation/libopforge-embedding-cookbook.md`
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace --all-targets -- -D warnings`; `cargo audit`; `make test`
  - Plan-compliance review evidence: `plan-compliance-reviewer` PASS citing the new prepared/report handle accessors, ABI additions, and focused tests
  - Commit outcome: planned: one commit that exposes prepared-session metadata accessors plus report-owned lockstep/trace traversal needed to match the stable Rust host data model
  - Definition of done: C callers can inspect prepared root module id, CPU name, source-map-like metadata, dependency files, and high-level report-owned trace/lockstep details without re-running assembly

- [ ] Item 3: Expose formatter parity through a dedicated FFI handle family
  - Source requirement or finding IDs: `libopforge::formatter` currently uncovered
  - Expected files: `crates/opforge-ffi/src/lib.rs`; `crates/opforge-ffi/opforge.h`; `crates/opforge-ffi/tests/abi_contract.rs`; `crates/opforge-ffi/tests/`; `documentation/libopforge-developer-guide.md`; `documentation/libopforge-embedding-cookbook.md`
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace --all-targets -- -D warnings`; `cargo audit`; `make test`
  - Plan-compliance review evidence: `plan-compliance-reviewer` PASS citing the formatter handle types, result model, and focused behavior coverage
  - Commit outcome: planned: one commit that introduces C-facing formatter config, execution, and report accessors aligned with the stable formatter facade
  - Definition of done: non-Rust hosts can format source and consume formatter diagnostics and summaries without depending on Rust wrappers

- [ ] Item 4: Expand registry FFI from enumeration to full stable capability and resolution workflows
  - Source requirement or finding IDs: `libopforge::registry` partial coverage
  - Expected files: `crates/opforge-ffi/src/lib.rs`; `crates/opforge-ffi/opforge.h`; `crates/opforge-ffi/tests/abi_contract.rs`; `crates/opforge-ffi/tests/`; `documentation/libopforge-developer-guide.md`
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace --all-targets -- -D warnings`; `cargo audit`; `make test`
  - Plan-compliance review evidence: `plan-compliance-reviewer` PASS citing the newly exposed resolution and capability helpers plus export/behavior coverage
  - Commit outcome: planned: one commit that adds CPU resolution, transition scanning, default-CPU lookup, and capability/cpusupport report access to the C layer
  - Definition of done: C callers can perform the same stable registry discovery and CPU-resolution tasks that current Rust hosts can perform through `libopforge::registry`

- [ ] Item 5: Add processor-neutral routing coverage for `libopforge::processing`
  - Source requirement or finding IDs: `libopforge::processing` currently underexposed
  - Expected files: `crates/opforge-ffi/src/lib.rs`; `crates/opforge-ffi/opforge.h`; `crates/opforge-ffi/tests/abi_contract.rs`; `crates/opforge-ffi/tests/`; `documentation/libopforge-diagnostics-and-fixits-guide.md`
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace --all-targets -- -D warnings`; `cargo audit`; `make test`
  - Plan-compliance review evidence: `plan-compliance-reviewer` PASS citing the routing entrypoints, error contract mapping, and focused tests across default/model/mode variants
  - Commit outcome: planned: one commit that exposes editor-routing and module-item-routing entrypoints plus neutral processing error traversal
  - Definition of done: C callers can route lines through the same stable processor-neutral boundary as Rust callers, including explicit model and execution-mode variants where the facade supports them

- [ ] Item 6: Complete `opcore` and `asm::opasm` parity for stable tooling-facing helpers
  - Source requirement or finding IDs: partial `opcore` and `asm::opasm` coverage
  - Expected files: `crates/opforge-ffi/src/lib.rs`; `crates/opforge-ffi/opforge.h`; `crates/opforge-ffi/tests/abi_contract.rs`; `crates/opforge-ffi/tests/`; `documentation/libopforge-developer-guide.md`
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace --all-targets -- -D warnings`; `cargo audit`; `make test`
  - Plan-compliance review evidence: `plan-compliance-reviewer` PASS citing the newly mapped line-parse, expression-token, portable, and processor-helper surfaces and their tests
  - Commit outcome: planned: one commit that rounds out the remaining stable opcore/opasm helpers, including portable forms and processor-builder-backed usage paths where appropriate for C
  - Definition of done: the tooling-facing stable facade exported under `libopforge::opcore` and `libopforge::asm::opasm` has a matching C representation or an explicit documented Rust-only exception list

- [ ] Item 7: Introduce a reusable FFI host-I/O contract that mirrors stable `io` concerns
  - Source requirement or finding IDs: weak `libopforge::io` parity
  - Expected files: `crates/opforge-ffi/src/lib.rs`; `crates/opforge-ffi/opforge.h`; `crates/opforge-ffi/tests/abi_contract.rs`; `crates/opforge-ffi/tests/`; `documentation/libopforge-embedding-cookbook.md`; `documentation/libopforge-developer-guide.md`
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace --all-targets -- -D warnings`; `cargo audit`; `make test`
  - Plan-compliance review evidence: `plan-compliance-reviewer` PASS citing the chosen source-provider/output-sink bridge shape, ownership rules, and host-facing examples
  - Commit outcome: planned: one commit that replaces ad hoc memory/file callback special cases with a reusable provider/sink bridge that better matches the Rust facade concepts
  - Definition of done: C callers have one coherent source/output abstraction story that can support filesystem-backed, memory-backed, and callback-backed hosts without inventing separate high-level workflows

- [ ] Item 8: Reconcile the high-level outcome model and publish full-surface FFI documentation
  - Source requirement or finding IDs: `dev-docs/NextSteps/libopforge_ffi_api_upgrade_spec_2026-03-17_rev1.md`; full-surface parity target in this plan
  - Expected files: `crates/opforge-ffi/src/lib.rs`; `crates/opforge-ffi/opforge.h`; `documentation/libopforge-developer-guide.md`; `documentation/libopforge-embedding-cookbook.md`; `documentation/libopforge-ffi-full-api-expansion-plan-v0_1.md`; optionally a replacement or revision of the March 17 FFI upgrade spec
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace --all-targets -- -D warnings`; `cargo audit`; `make test`; `cargo test -p ffi exported_release_ffi_library_exposes_full_header_symbol_surface --locked`
  - Plan-compliance review evidence: `plan-compliance-reviewer` PASS citing the final outcome contract, symbol-surface checks, and documentation sync
  - Commit outcome: planned: one commit that either upgrades the current report/result model consistently across the expanded surface or explicitly records why additive compatibility is retained for v0.1 rollout
  - Definition of done: the shipped header, Rust docs, cookbook, and FFI-specific spec all describe one coherent full-surface host API with no unresolved contract mismatches

## Milestones

- [ ] Milestone 1: surface inventory and metadata parity are complete after Items 1 and 2
- [ ] Milestone 2: formatter and registry parity are complete after Items 3 and 4
- [ ] Milestone 3: processing, opcore, and opasm tooling parity are complete after Items 5 and 6
- [ ] Milestone 4: host-I/O parity and final contract reconciliation are complete after Items 7 and 8

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- no new FFI concern should be declared complete without matching header,
  implementation, behavior tests, and export-surface validation