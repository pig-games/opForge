# libopforge API Aesthetics Implementation Plan — v0.1

## Goal
Turn `documentation/libopforge-api-aesthetics-improvement-plan-v0_1.md`
into an executable implementation plan with:

- ticket-sized work items
- strict commit gating
- explicit validation requirements
- explicit plan-compliance review requirements
- file-level touchpoints and closure criteria

This plan is for implementation sequencing, not for re-arguing the design.

## Governing documents
Every slice in this plan is governed by all of the following:

1. the active opForge agent instructions
2. `documentation/libopforge-api-aesthetics-improvement-plan-v0_1.md`
3. `agents/plan-compliance-reviewer.md`
4. any later user instruction that narrows scope further

The active opForge agent instructions must be included in the
plan-compliance review context. If they are not available to the reviewer, the
review must fail.

## Global execution rules
These rules apply to every ticket below.

### Rule 1: Small commits only
- each commit must close exactly one ticket or one explicitly pre-declared
  sub-ticket from this plan
- each commit must represent one conceptual change only
- if a ticket grows beyond a small, reviewable slice, stop and split the plan
  before continuing
- multi-ticket commits are not allowed

### Rule 2: No commit without full quality gates
Every commit candidate must pass the full local quality-gate suite:

- `cargo fmt --all --check`
- `cargo clippy -- -D warnings`
- `cargo audit`
- `cargo test --locked`
- `make test-vm-opasm-modes`
- `make test-build-profile-matrix`
- `make test-build-combo-smoke`

If any command fails, the commit is blocked.

### Rule 3: No commit without plan-compliance PASS
Before every commit, the sub-agent defined in
`agents/plan-compliance-reviewer.md` must be run and must return `PASS`.

The review context must include:

- the active opForge agent instructions
- this implementation plan
- `documentation/libopforge-api-aesthetics-improvement-plan-v0_1.md`
- the current ticket id and goal
- the exact files changed
- the consumer path affected
- the exact validation commands run and their results
- any temporary debt introduced

If the reviewer returns `FAIL`, the commit is blocked.

### Rule 4: No moving forward on a failed slice
- do not start the next ticket if the current ticket has not been committed
- do not mark progress in the plan unless the corresponding code/docs/tests are
  actually in place
- if a ticket needs to be split or re-ordered, update the plan first

### Rule 5: Progress checkboxes are mandatory bookkeeping
- every ticket in this plan must have a checkbox and that checkbox is part of
  the execution contract
- a ticket checkbox may be marked complete only after:
  - the scoped work is implemented
  - the full quality-gate suite passes
  - `agents/plan-compliance-reviewer.md` returns `PASS`
  - the ticket’s commit has been created
- checking off completed work is required before starting the next ticket
- no partial work may be checked off as done

### Rule 6: Docs follow the implemented surface
- for naming or API-surface changes, update the affected public docs in the
  same ticket unless the plan explicitly reserves them for the immediately
  following doc-only ticket
- use final names only; do not teach temporary spellings
- keep README, specification, developer guide, examples, and header/docs in
  sync with behavior

### Rule 7: `unstable` is not a destination
- no new export may be added to `libopforge::unstable`
- if a capability is worth keeping, it must be promoted to a stable home
- if it is not worth promoting, remove it from the facade

---

## Commit protocol
Every ticket follows this exact sequence:

1. select one open ticket from this plan
2. implement only that ticket’s scoped change
3. update any required docs/tests in the same slice
4. run the full quality-gate suite
5. run `agents/plan-compliance-reviewer.md` with the required context
6. commit only if all gates are green and the reviewer returns `PASS`
7. check off the completed ticket in this plan
8. update the plan state before beginning the next ticket

There are no exceptions in this phase.

---

## Workstream A — Direct naming cleanup

Progress checklist:
- [ ] AEST-001 Rename `opcore::normalized` to `opcore::portable`
- [ ] AEST-002 Rename `asm::opasm::normalized` to `asm::opasm::portable`
- [ ] AEST-003 Rename stable public `input_base` to `output_base`

### Ticket AEST-001: Rename `opcore::normalized` to `opcore::portable`
Objective:
- replace the stable public `normalized` submodule name in `libopforge::opcore`
  with `portable`

Primary files:
- `crates/opforge-lib/src/lib.rs`
- `documentation/libopforge-developer-guide.md`
- `documentation/libopforge-specification.md`
- `README.md`

Tests and validation focus:
- public facade tests for `opcore` portable tokenization and expression parsing
- doc/example search to ensure the stable surface no longer teaches
  `opcore::normalized`

Acceptance:
- public `libopforge::opcore::portable` exists
- public docs use `portable` for the stable module name
- no stable public example or doc teaches `opcore::normalized`

Commit size expectation:
- one small rename-focused commit

### Ticket AEST-002: Rename `asm::opasm::normalized` to `asm::opasm::portable`
Objective:
- replace the stable public `normalized` submodule name in
  `libopforge::asm::opasm` with `portable`

Primary files:
- `crates/opforge-lib/src/lib.rs`
- `documentation/libopforge-developer-guide.md`
- `documentation/libopforge-specification.md`

Tests and validation focus:
- public facade tests for portable statement parsing and portable processor use
- doc/example search to ensure the stable surface no longer teaches
  `asm::opasm::normalized`

Acceptance:
- public `libopforge::asm::opasm::portable` exists
- stable docs describe this as the portable contract view
- no stable public example or doc teaches `asm::opasm::normalized`

Commit size expectation:
- one small rename-focused commit

### Ticket AEST-003: Rename stable public `input_base` to `output_base`
Objective:
- replace the stable host-facing `input_base` vocabulary with `output_base`
  across the facade and its public docs

Primary files:
- `crates/opforge-lib/src/lib.rs`
- `examples/libopforge_in_memory.rs`
- `examples/libopforge_filesystem.rs`
- `documentation/libopforge-developer-guide.md`
- `documentation/libopforge-specification.md`
- `README.md`

Likely secondary touchpoints:
- `crates/opforge-engine/src/lib.rs`
- `crates/opforge-vm/src/output_model.rs`
- `crates/opforge-cli-core/src/cli.rs`
- `crates/opforge-ffi/src/lib.rs`
- `crates/opforge-ffi/opforge.h`

Implementation notes:
- use the final public name directly; do not keep compatibility aliases
- internal helper names may be renamed in the same ticket if that is the
  smallest compile-safe path
- any C-facing naming changed here must be reflected in header and FFI docs

Acceptance:
- the stable Rust facade and public docs use `output_base`
- example code uses `output_base`
- no stable public doc explains `input_base` as the preferred term

Commit size expectation:
- one rename-focused commit if it stays reviewable
- if the Rust and FFI rename together become too large, split into:
  - `AEST-003a` stable Rust surface
  - `AEST-003b` FFI/header/doc sync
  but only after this plan is updated first

---

## Workstream B — Example and doc ergonomics

Progress checklist:
- [ ] AEST-101 Convert in-memory example to builder-first final surface
- [ ] AEST-102 Convert filesystem example to builder-first final surface
- [ ] AEST-103 Rewrite developer guide high-level embedding path
- [ ] AEST-104 Final public doc wording pass after naming rewrite

### Ticket AEST-101: Convert in-memory example to builder-first final surface
Objective:
- rewrite `examples/libopforge_in_memory.rs` to teach the owned/session builder
  using the final names from Workstream A

Primary files:
- `examples/libopforge_in_memory.rs`

Acceptance:
- the example leads with `AssemblerSession::builder(...)`
- it uses final names such as `output_base`
- it remains a copy-paste-quality primary embedding example

### Ticket AEST-102: Convert filesystem example to builder-first final surface
Objective:
- rewrite `examples/libopforge_filesystem.rs` to teach the owned/session
  builder using the final names from Workstream A

Primary files:
- `examples/libopforge_filesystem.rs`

Acceptance:
- the example leads with `AssemblerSession::builder(...)`
- it uses final naming and does not center a large grouped config literal

### Ticket AEST-103: Rewrite developer guide high-level embedding path
Objective:
- make the first developer-guide examples and ownership explanation teach the
  intended builder-first surface

Primary files:
- `documentation/libopforge-developer-guide.md`

Implementation notes:
- add a short borrowed-vs-owned ownership choice table
- keep one compact grouped-config example for hosts mapping from an existing
  config model

Acceptance:
- the first owned-session examples are builder-first
- the guide teaches the final names only
- the grouped config example is secondary rather than the first thing seen

### Ticket AEST-104: Final public doc wording pass after naming rewrite
Objective:
- align README and specification wording with the final public names and
  builder-first story

Primary files:
- `README.md`
- `documentation/libopforge-specification.md`

Acceptance:
- README and specification no longer explain transitional naming
- examples and wording are consistent with Workstream A and Workstream B

---

## Workstream C — Borrowed-builder parity

Progress checklist:
- [ ] AEST-201 Add source-configuration parity to `AssemblerBuilder`
- [ ] AEST-202 Add execution-configuration parity to `AssemblerBuilder`
- [ ] AEST-203 Add output-routing parity to `AssemblerBuilder`
- [ ] AEST-204 Add diagnostics and output-policy parity to `AssemblerBuilder`

### Ticket AEST-201: Add source-configuration parity to `AssemblerBuilder`
Objective:
- add borrowed-builder support for source concerns already supported by
  `AssemblerSessionBuilder`

Primary files:
- `crates/opforge-lib/src/lib.rs`

Scope:
- `defines(...)`
- `include_paths(...)`
- `module_paths(...)`
- `pp_macro_depth(...)`

Acceptance:
- borrowed and owned builders support equivalent source-shaping concerns

### Ticket AEST-202: Add execution-configuration parity to `AssemblerBuilder`
Objective:
- add the missing borrowed-builder execution controls

Primary files:
- `crates/opforge-lib/src/lib.rs`

Scope:
- `cpu_override(...)`
- `max_loop_iterations(...)`

Acceptance:
- borrowed and owned builders no longer diverge on normal execution controls

### Ticket AEST-203: Add output-routing parity to `AssemblerBuilder`
Objective:
- add the missing borrowed-builder output-routing and naming controls

Primary files:
- `crates/opforge-lib/src/lib.rs`

Scope:
- `go_addr(...)`
- `bin_specs(...)`
- `fill_byte(...)`
- `labels_file(...)`
- `dependency_output(...)`
- `outfile_override(...)`
- `list_name_override(...)`
- `hex_name_override(...)`

Acceptance:
- borrowed and owned builders are aligned for output-routing concerns

### Ticket AEST-204: Add diagnostics and output-policy parity to `AssemblerBuilder`
Objective:
- close the remaining borrowed-builder surface gap for diagnostics and output
  policy controls

Primary files:
- `crates/opforge-lib/src/lib.rs`

Scope:
- diagnostics toggles
- default/no-output controls if they still exist under those names at this
  point in the plan

Acceptance:
- borrowed and owned builders differ primarily by ownership, not by API grammar

---

## Workstream D — Output-policy vocabulary cleanup

Progress checklist:
- [ ] AEST-301 Replace `suppress_outputs` with an intent-expressive name
- [ ] AEST-302 Replace ambiguous `default_outputs` boolean if warranted

### Ticket AEST-301: Replace `suppress_outputs` with an intent-expressive name
Objective:
- replace the mechanically named no-output control with the final public name

Primary files:
- `crates/opforge-lib/src/lib.rs`
- `crates/opforge-engine/src/lib.rs`
- `crates/opforge-ffi/src/lib.rs`
- `crates/opforge-ffi/opforge.h`
- `README.md`
- `documentation/libopforge-specification.md`
- `documentation/libopforge-developer-guide.md`

Target direction:
- `no_outputs`

Acceptance:
- public API and docs use the final no-output term directly
- no stable public doc teaches `suppress_outputs` as the preferred name

### Ticket AEST-302: Replace ambiguous `default_outputs` boolean if warranted
Objective:
- determine whether `default_outputs` remains acceptable after the rest of the
  surface is cleaned up; if not, replace it with a clearer final control

Primary files:
- `crates/opforge-lib/src/lib.rs`
- any engine/FFI/doc touchpoints required by the chosen final shape

Decision rule:
- if the boolean still reads mechanically after `no_outputs` lands, replace it
- if it becomes clear enough in context, record that decision and close the
  ticket with no rename

Acceptance:
- the final output-policy vocabulary reads like host intent rather than wiring

---

## Workstream E — Drain `unstable`

Progress checklist:
- [ ] AEST-401 Decide stable home for formatter exports
- [ ] AEST-402 Resolve registry-report helper fate
- [ ] AEST-403 Remove redundant stable spillover from `unstable`
- [ ] AEST-404 Remove raw engine/request spillover from the facade

### Ticket AEST-401: Decide stable home for formatter exports
Objective:
- move formatter exports out of `unstable` into a stable module or document
  their removal from the facade

Primary files:
- `crates/opforge-lib/src/lib.rs`
- `crates/opforge-cli/src/lib.rs`

Acceptance:
- formatter-facing consumers no longer depend on `libopforge::unstable`
- the stable destination module is explicit

### Ticket AEST-402: Resolve registry-report helper fate
Objective:
- either promote `capabilities_report*` and `cpusupport_report*` to a stable
  module home or remove them from the facade and switch internal consumers to
  lower-level crates

Primary files:
- `crates/opforge-lib/src/lib.rs`
- `crates/opforge-cli/src/lib.rs`

Acceptance:
- the decision is reflected in code and docs
- no internal consumer still depends on these APIs through `unstable`

### Ticket AEST-403: Remove redundant stable spillover from `unstable`
Objective:
- delete `unstable` re-exports already covered by stable modules such as
  `processing` and `registry`

Primary files:
- `crates/opforge-lib/src/lib.rs`
- any internal consumer files still using those `unstable` names

Acceptance:
- duplicated stable exports are removed from `unstable`
- internal callers use the stable module home directly

### Ticket AEST-404: Remove raw engine/request spillover from the facade
Objective:
- delete the remaining raw engine/request exports from the facade when their
  only meaningful consumers are internal workspace crates

Primary files:
- `crates/opforge-lib/src/lib.rs`
- `crates/opforge-cli/src/bin/build_vm_package.rs`
- `crates/opforge-cli-core/src/cli.rs`

Acceptance:
- internal tooling depends on lower-level crates directly where appropriate
- `libopforge::unstable` is empty or gone

---

## Milestones and exit gates

### Milestone 0: Naming complete
- [ ] Milestone 0 reached
- `AEST-001` through `AEST-003` complete
- exit gate:
  - final names are in code and public docs
  - no compatibility aliases were introduced

### Milestone 1: Public teaching surface complete
- [ ] Milestone 1 reached
- `AEST-101` through `AEST-104` complete
- exit gate:
  - examples and docs teach the final builder-first surface

### Milestone 2: Borrowed-builder parity complete
- [ ] Milestone 2 reached
- `AEST-201` through `AEST-204` complete
- exit gate:
  - borrowed and owned builders differ mainly by ownership

### Milestone 3: Output-policy vocabulary complete
- [ ] Milestone 3 reached
- `AEST-301` and `AEST-302` complete
- exit gate:
  - output controls read as host intent, not internal mechanics

### Milestone 4: `unstable` drained
- [ ] Milestone 4 reached
- `AEST-401` through `AEST-404` complete
- exit gate:
  - `libopforge::unstable` is removed, or left empty pending immediate removal

---

## Done definition
This plan is complete only when:

- every committed slice passed the full quality-gate suite
- every committed slice passed `agents/plan-compliance-reviewer.md`
- the stable facade teaches final names only
- the examples and developer docs teach the builder-first intended path
- borrowed and owned builders feel like one API family
- `libopforge::unstable` is not a live overflow namespace anymore
