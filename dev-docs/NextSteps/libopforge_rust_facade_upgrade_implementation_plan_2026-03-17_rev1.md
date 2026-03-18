# libopforge Rust Facade Upgrade Implementation Plan

Date: 2026-03-17
Plan revision: rev1
Mode: implementation-first, commit-gated execution plan
Source spec: `dev-docs/NextSteps/libopforge_rust_facade_upgrade_spec_2026-03-17_rev7.md`
Companion spec: `dev-docs/NextSteps/libopforge_ffi_api_upgrade_spec_2026-03-17_rev1.md`
Scope: the published `libopforge` Rust facade crate and only the lower-layer changes required to make that facade contract true

## Goal

Translate the rev7 Rust facade specification into a small-commit implementation sequence that:

- preserves the current public workflow names, config names, builder/session lifecycle, and module tree
- introduces the named `CoreError` and `ProcessorError` domains without flattening the architecture
- adds an assembler-workflow error surface only at the high-level packaging boundary
- keeps `engine` processor-neutral and exported through `libopforge::processing`
- documents canonical versus compatibility exports and the concern inventory of every published facade module
- requires a full repository quality gate before every commit and forbids overlap between slices

## Scope lock

This plan follows the active worktree rules in `AGENTS.md`.

- Production behavior must advance in every implementation session.
- Each work package below is one commit-sized slice.
- Do not start the next work package until the current work package is committed.
- Before every commit, the full quality gate must pass.
- Do not broaden the task into formatter-taxonomy redesign, plugin architecture redesign, or unrelated facade cleanup.
- Do not plan downstream-crate edits as milestones for this spec. If fallout appears in `ffi`, `lsp`, or CLI crates, treat that as discovered breakage after the relevant facade slice lands rather than as planned scope.
- Prefer additive compatibility shims first, then tighten canon/compat policy once the new stable surface is in place.
- During implementation sessions, use the required execution header and progress log format from `AGENTS.md`.

## Definition of done

This plan is complete when all of the following are true:

- `libopforge::opcore` exports a named `CoreError` and `CoreErrorKind` with the required inspection and conversion surface.
- Pure `opcore` expression and other generic `opcore` concern failures are classified under the core domain rather than assembler-owned umbrellas.
- `libopforge::processing` exports `ProcessorError`, `ProcessorErrorKind`, and `ProcessorFailureDetail` as the stable neutral processor-failure surface.
- High-level assembler workflows expose an assembler-workflow-specific error surface without forcing assembler-owned types into `opcore` or the neutral engine boundary.
- The published concern inventory for `asm`, `asm::opasm`, `formatter`, `processing`, `registry`, `lockstep`, `io`, `diagnostics`, and `opcore` is explicit in code/docs.
- Canonical versus compatibility-oriented exports are documented and enforced by an export audit.
- Every work package was committed only after the full quality gate passed.

## Progress checklist

- [x] `LFR-001` Add `CoreError` and `CoreErrorKind` with leaf conversions
- [x] `LFR-002` Classify module, `.use`, and import failures under `CoreError`
- [x] `LFR-003` Classify macro, conditional, and repetition failures under `CoreError`
- [ ] `LFR-004` Classify namespace, scope, and preprocess failures under `CoreError`
- [ ] `LFR-005` Classify struct and segment failures under `CoreError` and remove pure-`opcore` assembler-error coupling
- [ ] `LFR-006` Add neutral `ProcessorError` surface in `libopforge::processing`
- [ ] `LFR-007` Add `AssemblerWorkflowError` at the high-level asm boundary
- [ ] `LFR-008` Complete workflow mappings and facade-local stability regressions
- [ ] `LFR-009` Add canon/compat export audit enforcement
- [ ] `LFR-010` Publish concern inventories in the facade surface and guide
- [ ] `LFR-011` Add guide and rustdoc example coverage for the stabilized entry paths
- [ ] Milestone A exit gate: named core domain is established and truthful
- [ ] Milestone B exit gate: neutral orchestration and workflow packaging are established
- [ ] Milestone C exit gate: public-shape policy and documentation are explicit

Tracking note:
- Mark a package checkbox when its commit lands.
- Mark the task and validation checkboxes inside a package while that slice is in flight.
- Mark milestone exit gates only after every included package is complete and the exit condition is true.

## Required commit discipline

Every work package uses the same cadence:

- [x] Make the smallest production-code change that advances the current slice.
- [x] Add or update only the focused validation needed for that slice.
- [x] Run targeted tests for the touched area until the slice is stable.
- [x] Run the full pre-commit quality gate from the worktree root:
   - [x] `cargo fmt --all`
   - [x] `cargo clippy --workspace -- -D warnings`
   - [x] `cargo audit`
   - [x] `cargo test --locked --workspace`
- [ ] Commit the slice.
- [ ] Only then begin the next work package.

If a slice changes examples or checked-in reference artifacts, run tests first, update references only for intentional deltas, then rerun the affected tests before the full gate.

## Source map

| Spec theme | Plan slice |
|---|---|
| Named `opcore` error domain | `LFR-001` |
| `CoreErrorKind` concern coverage | `LFR-002`, `LFR-003`, `LFR-004`, `LFR-005` |
| Pure expression ownership stays in `opcore` | `LFR-005` |
| Neutral `engine` processor failure surface | `LFR-006` |
| `EngineError` local naming rule, if applicable | `LFR-006` |
| High-level assembler workflow error surface | `LFR-007`, `LFR-008` |
| Workflow/config compile-usage preservation | `LFR-001`, `LFR-008` |
| Diagnostic structural-consistency regression | `LFR-008` |
| Canonical versus compatibility exports | `LFR-009` |
| Concern inventories across facade modules | `LFR-010` |
| Guide/rustdoc/examples | `LFR-011` |

## Work packages

### LFR-001: Add `CoreError` and `CoreErrorKind` with leaf conversions

Objective:
- introduce the first real public `CoreError` surface in `libopforge::opcore` without renaming the existing workflow/config APIs

Why this slice comes first:
- it creates the public type anchor the rest of the plan depends on while keeping the change additive and narrow

Primary files:
- `crates/opforge-core/src/` error and service modules that currently surface `TokenizeError`, `ParseError`, and `EvalError`
- `crates/opforge-lib/src/lib.rs`
- `crates/opforge-lib/tests/` or equivalent facade-usage test modules

Implementation tasks:
- [x] introduce `CoreError` and `CoreErrorKind` in the canonical `opcore` public surface
- [x] implement `kind()`, `summary()`, and `code()`
- [x] add `From<TokenizeError>`, `From<ParseError>`, and `From<EvalError>`
- [x] keep existing leaf APIs precise where the spec permits direct leaf returns
- [x] add compile-time or usage-style tests that the preserved named workflow/config surface still compiles unchanged

Focused validation:
- [x] unit tests for `CoreErrorKind` classification of tokenize, parse, and eval failures
- [x] usage tests proving leaf errors still convert cleanly into `CoreError`
- [x] compile-usage tests for the preserved workflow/config names listed by the spec

Commit exit gate:
- [x] `cargo test --locked -p opforge-core`
- [x] `cargo test --locked -p libopforge`
- [x] full pre-commit quality gate

Definition of done:
- downstream Rust code can depend on a named `libopforge::opcore::CoreError` immediately after this commit

Recommended commit outcome:
- `Add CoreError and CoreErrorKind to libopforge opcore`

---

### LFR-002: Classify module, `.use`, and import failures under `CoreError`

Objective:
- make the first non-leaf generic `opcore` concerns report through the new core domain

Why this slice is narrow enough:
- it stays inside one coherent concern family around module graph entry and import-oriented core processing

Primary files:
- `crates/opforge-core/src/services.rs`
- `crates/opforge-core/src/` modules that own module-item parsing, `.use`, and import behavior
- `crates/opforge-vm/src/vm_opcore.rs`

Implementation tasks:
- [x] classify public module, `.use`, and import failures under `CoreErrorKind`
- [x] add any dedicated core leaf wrappers only if they remain canonical members of `CoreError`
- [x] preserve successful portable and non-portable `process_module_item` behavior

Focused validation:
- [x] tests for module-item, `.use`, and import failures mapping to `CoreErrorKind`
- [x] regression tests for successful `process_module_item` paths across native and portable helpers

Commit exit gate:
- [x] `cargo test --locked -p opforge-core`
- [x] `cargo test --locked -p opforge-vm`
- [x] `cargo test --locked -p libopforge`
- [x] full pre-commit quality gate

Definition of done:
- the public module-entry concern family no longer escapes through assembler-owned umbrellas

Recommended commit outcome:
- `Classify module and import failures under CoreError`

---

### LFR-003: Classify macro, conditional, and repetition failures under `CoreError`

Objective:
- cover the first generic control-flow group owned by `opcore`

Why this slice is separate:
- macro expansion and conditional/repetition handling are related enough to land together, but they are still narrower than the full remaining concern inventory

Primary files:
- `crates/opforge-core/src/` modules that own macro processing, conditionals, and repetition behavior
- `crates/opforge-vm/src/vm_opcore.rs` if the VM path mirrors these concern classifications

Implementation tasks:
- [x] classify macro, conditional, and repetition failures under `CoreErrorKind`
- [x] keep leaf wrappers core-owned where dedicated wrappers are needed
- [x] avoid widening this slice into namespace, scope, preprocess, struct, or segment behavior

Focused validation:
- [x] focused tests for macro, conditional, and repetition failure classification
- [x] audit-style checks proving these public failure paths classify under `CoreError` rather than assembler-owned umbrellas

Commit exit gate:
- [x] `cargo test --locked -p opforge-core`
- [x] `cargo test --locked -p opforge-vm`
- [x] `cargo test --locked -p libopforge`
- [x] full pre-commit quality gate

Definition of done:
- the generic macro and control-loop concern family is truthfully owned by `CoreError`

Recommended commit outcome:
- `Classify macro and control failures under CoreError`

---

### LFR-004: Classify namespace, scope, and preprocess failures under `CoreError`

Objective:
- cover the remaining generic language-structure management concerns owned by `opcore`

Why this slice is separate:
- namespace/scope/preprocess behavior touches a different set of entrypoints and failure sites than macro/control handling

Primary files:
- `crates/opforge-core/src/` modules that own namespace handling, scope handling, and preprocess behavior
- `crates/opforge-vm/src/vm_opcore.rs` if the VM path mirrors these concern classifications

Implementation tasks:
- [ ] classify namespace, scope, and preprocess failures under `CoreErrorKind`
- [ ] keep leaf wrappers core-owned where dedicated wrappers are needed
- [ ] avoid widening this slice into struct, segment, or dependency-cleanup work

Focused validation:
- [ ] focused tests for namespace, scope, and preprocess failure classification
- [ ] audit-style checks proving these public failure paths classify under `CoreError` rather than assembler-owned umbrellas

Commit exit gate:
- [ ] `cargo test --locked -p opforge-core`
- [ ] `cargo test --locked -p opforge-vm`
- [ ] `cargo test --locked -p libopforge`
- [ ] full pre-commit quality gate

Definition of done:
- the generic namespace/scope/preprocess concern family is truthfully owned by `CoreError`

Recommended commit outcome:
- `Classify scope and preprocess failures under CoreError`

---

### LFR-005: Classify struct/segment concerns and remove pure-`opcore` assembler-error coupling

Objective:
- finish the remaining `opcore` concern coverage and enforce the pure-expression ownership rule

Why this slice is separate:
- it closes the last core-owned failure families and isolates any invasive dependency cleanup from the earlier classification slices

Primary files:
- `crates/opforge-core/src/` modules that own struct and segment behavior
- pure-expression helper modules under `crates/opforge-core/src/`
- `crates/opforge-lib/src/lib.rs` if the public re-exports need adjustment

Implementation tasks:
- [ ] classify struct and segment failures under `CoreErrorKind`
- [ ] remove any remaining pure-`opcore` dependency on `AsmError` or `AsmErrorKind` as the source error domain
- [ ] add audit or compile checks proving public `opcore` helpers do not require assembler-facing workflow error types

Focused validation:
- [ ] tests for struct and segment failure classification
- [ ] audit-style checks proving pure expression helpers do not depend on `AsmError` or `AsmErrorKind`
- [ ] audit-style checks proving public `opcore` APIs do not require assembler workflow errors

Commit exit gate:
- [ ] `cargo test --locked -p opforge-core`
- [ ] `cargo test --locked -p libopforge`
- [ ] full pre-commit quality gate

Definition of done:
- all spec-listed public `opcore` concern families now classify under `CoreError`, and pure core expression services no longer source their failures from assembler-owned types

Recommended commit outcome:
- `Finish CoreError concern coverage in opcore`

---

### LFR-006: Add neutral `ProcessorError` surface in `libopforge::processing`

Objective:
- create the stable processor-neutral failure contract used when processor-local failures cross the internal `engine` boundary

Why this slice is separate:
- it is the first place where the plan intentionally erases processor-local typing, so it should be isolated from the high-level assembler packaging work

Primary files:
- `crates/opforge-engine/src/lib.rs`
- `crates/opforge-types/src/processing.rs`
- `crates/opforge-lib/src/lib.rs`

Implementation tasks:
- [ ] introduce `ProcessorError`, `ProcessorErrorKind`, and `ProcessorFailureDetail`
- [ ] keep `CoreError` pass-through available for genuinely generic-core failures
- [ ] type-erase processor-specific failures into the neutral representation when they cross the engine boundary
- [ ] ensure the stable public contract is exported from `libopforge::processing`, not from a new top-level `engine` module
- [ ] if this slice retains or introduces an engine-internal umbrella error type, enforce the local `EngineError` and `EngineErrorKind` naming rule here; otherwise record that no such public-facing engine umbrella exists

Focused validation:
- [ ] tests proving core-local parse/tokenize/eval and other generic `opcore` failures can still cross as `CoreError`
- [ ] tests proving processor-local failures cross as `ProcessorError` rather than concrete processor enums
- [ ] tests proving the neutral surface exposes stable category, code, summary, and structured-detail inspection
- [ ] audit-style checks proving the public neutral boundary does not require concrete processor-specific enums

Commit exit gate:
- [ ] `cargo test --locked -p opforge-engine`
- [ ] `cargo test --locked -p libopforge`
- [ ] full pre-commit quality gate

Definition of done:
- the processor-neutral orchestration surface no longer requires concrete `opasm` or future processor-specific enums in its public contract

Recommended commit outcome:
- `Add neutral ProcessorError surface to libopforge processing`

---

### LFR-007: Add `AssemblerWorkflowError` at the high-level asm boundary

Objective:
- give `libopforge::asm` an explicit workflow-oriented error type at the correct packaging boundary

Why this slice follows `LFR-006`:
- the assembler workflow layer should translate from the already-stable core and neutral engine domains rather than inventing them inline

Primary files:
- `crates/opforge-lib/src/lib.rs`
- `crates/opforge-engine/src/lib.rs`
- `crates/opforge-asm/src/error.rs`

Implementation tasks:
- [ ] introduce `AssemblerWorkflowError` and its classifier at the high-level assembler packaging boundary
- [ ] wire the minimal borrowed assembly path through the new workflow surface
- [ ] preserve `AsmRunError`, `AsmRunReport`, `Diagnostic`, and `AsmError` as domain-local diagnostics/reporting types rather than flattening them into one crate-global umbrella
- [ ] keep `check()`, `prepare()`, and `assemble()` naming and lifecycle unchanged

Focused validation:
- [ ] tests for the new workflow error type on the borrowed high-level path
- [ ] tests for the minimal mapping path from `CoreError` and `ProcessorError` into `AssemblerWorkflowError`

Commit exit gate:
- [ ] `cargo test --locked -p opforge-engine`
- [ ] `cargo test --locked -p libopforge`
- [ ] full pre-commit quality gate

Definition of done:
- the high-level assembler boundary owns a new workflow wrapper type and at least one real top-level path now returns it

Recommended commit outcome:
- `Add AssemblerWorkflowError to libopforge asm`

---

### LFR-008: Complete workflow mappings and facade-local stability regressions

Objective:
- finish the remaining workflow mappings and stability coverage after the wrapper type exists

Why this slice is separate:
- once the type exists, the broader mapping matrix and regression coverage can land as a follow-up commit without turning the initial wrapper introduction into an oversized slice

Primary files:
- `crates/opforge-lib/src/lib.rs`
- `crates/opforge-engine/src/lib.rs`
- `crates/opforge-asm/src/error.rs`
- `crates/opforge-lib/tests/` or equivalent facade regression test modules

Implementation tasks:
- [ ] define the remaining stable mappings from `CoreError`, `ProcessorError`, `AsmRunError`, I/O failures, and internal failures into the workflow surface
- [ ] cover the owned workflow path as well as the borrowed path
- [ ] add a dedicated regression proving `Diagnostic` remains structurally consistent for Rust consumers

Focused validation:
- [ ] tests for each `AssemblerWorkflowError` category on the high-level borrowed and owned workflow paths
- [ ] tests for `AsmRunError` accessors and `From<AsmRunError> for AssemblerWorkflowError`
- [ ] regression tests proving existing successful assemble/check flows keep the current user-facing result shapes where the spec says to preserve them
- [ ] regression tests proving `Diagnostic` structure remains stable

Commit exit gate:
- [ ] `cargo test --locked -p opforge-engine`
- [ ] `cargo test --locked -p libopforge`
- [ ] full pre-commit quality gate

Definition of done:
- the high-level assembler packaging boundary now has complete workflow mappings and stability coverage while lower layers retain their own truthful local error typing

Recommended commit outcome:
- `Complete libopforge workflow error mappings and regressions`

---

### LFR-009: Add canon/compat export audit enforcement

Objective:
- make overlapping exports an enforced policy rather than a documentation convention

Why this slice is separate:
- the export audit should land only after the new stable types and module boundaries exist, and it should not be mixed with guide prose changes

Primary files:
- `crates/opforge-lib/src/lib.rs`
- export-audit test or report infrastructure under the libopforge worktree

Implementation tasks:
- [ ] list overlapping public exports
- [ ] name one canonical module for each overlapping concept
- [ ] mark every other overlapping export as compatibility-oriented
- [ ] fail validation when a newly overlapping export appears without an explicit audit decision

Focused validation:
- [ ] export-audit test or generated report validation
- [ ] facade tests proving the canonical homes for the new core and processing error families

Commit exit gate:
- [ ] `cargo test --locked -p libopforge facade_export_audit`
- [ ] `cargo test --locked -p libopforge`
- [ ] full pre-commit quality gate

Definition of done:
- the public surface now has an enforceable canon/compat policy instead of an implied one

Recommended commit outcome:
- `Enforce canon and compatibility export audit for libopforge`

---

### LFR-010: Publish concern inventories in the facade surface and guide

Objective:
- make the architecture legible by publishing explicit concern inventories for every stable facade module

Why this slice is separate:
- concern inventories are part of the public contract, but they should land before example expansion so the examples target stable documented ownership boundaries

Primary files:
- `crates/opforge-lib/src/lib.rs`
- `documentation/libopforge-developer-guide.md`

Implementation tasks:
- [ ] document the concern inventory for `asm`, `asm::opasm`, `formatter`, `processing`, `registry`, `lockstep`, `io`, `diagnostics`, and `opcore`
- [ ] add the explicit note that CLI or host presentation may specialize wording more than lower API layers do
- [ ] keep this slice limited to ownership and concern-inventory publication, not example expansion

Focused validation:
- [ ] guide review against the new concern inventories and public-shape policy
- [ ] a targeted doc-surface regression such as `cargo test --locked -p libopforge facade_concern_inventory_docs`

Commit exit gate:
- [ ] `cargo test --locked -p libopforge facade_concern_inventory_docs`
- [ ] `cargo test --locked -p libopforge`
- [ ] full pre-commit quality gate

Definition of done:
- the facade and guide explicitly teach which stable module owns which concern boundary

Recommended commit outcome:
- `Document libopforge concern inventories`

---

### LFR-011: Add guide and rustdoc example coverage for stabilized entry paths

Objective:
- add focused examples and compilation-backed documentation coverage for the stabilized facade entry paths

Why this slice is last:
- examples should be written against the finalized concern inventories, error domains, and export policy

Primary files:
- `documentation/libopforge-developer-guide.md`
- `documentation/libopforge-developer-guide-examples/` as needed
- rustdoc examples in `crates/opforge-lib/src/lib.rs` as needed

Implementation tasks:
- [ ] add or update examples for borrowed, owned, prepared, `opcore`-only, assembler-workflow, `asm::opasm`, formatter, registry, and lockstep entry paths
- [ ] keep the examples consumer-shaped but facade-local; do not turn this slice into downstream-crate remediation
- [ ] ensure example wording and code reflect the canon/compat policy and the finalized workflow/error boundaries

Focused validation:
- [ ] `cargo test --locked -p libopforge --doc`
- [ ] `cargo test --locked -p libopforge facade_guide_examples`

Commit exit gate:
- [ ] `cargo test --locked -p libopforge --doc`
- [ ] `cargo test --locked -p libopforge facade_guide_examples`
- [ ] `cargo test --locked -p libopforge`
- [ ] full pre-commit quality gate

Definition of done:
- the documented facade entry paths are covered by compilation-backed examples instead of prose-only guidance

Recommended commit outcome:
- `Add libopforge guide and rustdoc example coverage`

## Milestones and stop points

### Milestone A: Named core domain established

Includes:
- [ ] `LFR-001`
- [ ] `LFR-002`
- [ ] `LFR-003`
- [ ] `LFR-004`
- [ ] `LFR-005`

Exit gate:
- every spec-listed public `opcore` concern family now has truthful core-domain ownership

### Milestone B: Neutral orchestration and workflow packaging established

Includes:
- [ ] `LFR-006`
- [ ] `LFR-007`
- [ ] `LFR-008`

Exit gate:
- `engine` stays processor-neutral while high-level assembler workflows own their own wrapper surface

### Milestone C: Public-shape policy and documentation established

Includes:
- [ ] `LFR-009`
- [ ] `LFR-010`
- [ ] `LFR-011`

Exit gate:
- the facade’s export policy, concern inventories, and examples all match the intended stable surface

## Explicit non-goals during implementation

- no rename sweep for existing config or builder types
- no new public top-level `libopforge::engine` module
- no formatter-specific error-taxonomy expansion in this revision
- no compatibility-export removals done only for aesthetics
- no unrelated cleanup in lower-level crates unless it directly unblocks the current slice

## Validation notes and risk controls

- Use focused tests first to keep iteration fast, but do not commit without the full gate.
- If a slice unexpectedly requires reference updates or fixture churn, stop and prove the behavioral change is intentional before updating anything.
- If a lower-layer change threatens to widen into a broader architecture rewrite, cut the slice back to the smallest vertical path that still advances the spec.
- If a blocker appears, report the exact module, error boundary, attempted path, and smallest next step instead of filling time with cleanup.

## Open decisions held constant for this plan

- Formatter stays on its current error taxonomy in this revision; only its concern inventory and public entry path documentation are updated.
- Compatibility re-exports remain available until the canon/compat audit in `LFR-009` makes explicit decisions about each overlap.
- Any engine-internal umbrella error that remains or is introduced should follow the local `EngineError` naming rule, but the stable external neutral-failure contract is still exported through `libopforge::processing`.