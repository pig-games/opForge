# Native AmigaOS Module/Use Port Implementation Plan v0.1

## Metadata

- Source: `documentation/opForge-native-amigaos-module-use-port-spec-v0_1.md`
- Mode: implementation
- Owner: Codex / opForge native AmigaOS implementation

## Objective

Port the Rust `.module`/`.use` infrastructure into the 68020 AmigaOS-native
CLI path in small vertical slices. The first working target is native table
state for module declarations, module endings, module paths, and simple imports,
with deterministic diagnostics and FS-UAE validation for runtime behavior.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Production code must move first in each implementation slice.
- Each work item ends in exactly one new commit before the next item starts.
- Parser VM, module graph, macro injection, and emission work must stay
  separated unless a direct blocker requires a narrower integration change.
- The native implementation must stay close to Rust CLI spelling and
  module/use syntax, but may explicitly support a smaller subset per slice.
- Native data structures must be fixed-capacity and must fail deterministically
  on overflow.
- Existing unrelated worktree changes must not be reverted.

## Work Items

- [x] Item 0: Land transitional tokenizer-post module/use visibility scanner
  - Validation: focused native CLI host tests, reference gate, smoke fixture
    staging test, and format check.
  - Definition of done: native CLI reports `.module <id>` and `.use <id>`
    visibility after tokenizer success without claiming table-backed module/use
    parity.
  - Source requirement or finding IDs: Roadmap current-baseline note for
    in-flight separate-agent hard-coded `.module` / `.use` parsing and spec
    problem statement describing the transitional hard-coded scan.
  - Expected files: `examples/motorola68000/amigaos/opforge/opforge_cli.asm`,
    `crates/opforge-asm/src/tests.rs`,
    `crates/opforge-asm/src/fs_uae_smoke.rs`, reference artifacts when native
    CLI output changes.
  - Detailed validation: `cargo fmt --all --check`; `cargo test -p asm
    motorola68020_opforge_native_cli -- --nocapture`; `cargo test -p asm
    examples_match_reference_outputs -- --nocapture`; `cargo test -p asm
    fs_uae_smoke::tests::example_guest_input_exposes_smoke_source_files --
    --nocapture`.
  - Full quality gates: scanner runs only after tokenizer success for a line;
    `.module main` and `.use math` produce deterministic report markers;
    emitter remains the explicit not-implemented boundary; table-backed
    records, `.endmodule`, module paths, and module resolution remain deferred
    to later items.
  - Plan-compliance review evidence: run `plan-compliance-reviewer` before
    commit and record PASS.
  - Commit outcome: one commit landing the transitional module/use visibility
    scanner and keeping all table-backed companion-plan items unchecked.
  - Detailed definition of done: the native CLI has a narrow, auditable
    bootstrap visibility baseline that can be cleanly separated from the later
    `.include` input-expansion slice.

- [x] Item 1: Add native module/use state tables
  - Validation: focused native CLI host tests, reference gate, and format check.
  - Definition of done: native CLI initializes fixed-capacity module/import/path
    state and tests lock the labels/capacities.
  - Source requirement or finding IDs: Spec goals for module/import tables and
    deterministic overflow behavior.
  - Expected files: `examples/motorola68000/amigaos/opforge/opforge_cli.asm`,
    `crates/opforge-asm/src/tests.rs`,
    `examples/reference/motorola68000/amigaos/opforge/opforge_cli.hunk`,
    `examples/reference/motorola68000/amigaos/opforge/opforge_cli.lst`.
  - Detailed validation: `cargo fmt --all --check`; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; `cargo test -p asm
    examples_match_reference_outputs -- --nocapture`.
  - Full quality gates: host assembly/surface tests green; reference output
    updated only for expected native CLI output drift.
  - Plan-compliance review evidence: run the branch-local
    `plan-compliance-reviewer` against this item before commit and record PASS
    in the commit notes or handoff.
  - Commit outcome: one commit adding fixed-capacity native module/import table
    storage and reset/init logic, without changing directive behavior yet.
  - Detailed definition of done: native CLI has zeroed module/import/path state at run
    start and host tests lock table labels/capacities.

- [x] Item 2: Replace print-only `.module` recognition with table-backed module
      records
  - Validation: focused native CLI host tests, reference gate, format check, and
    opt-in FS-UAE success/failure smoke when available.
  - Definition of done: module summaries are emitted from native module table
    state and malformed module declarations fail before emitter stage.
  - Source requirement or finding IDs: Spec `.module` behavioral contract and
    `.endmodule` boundary cases.
  - Expected files: `examples/motorola68000/amigaos/opforge/opforge_cli.asm`,
    `crates/opforge-asm/src/tests.rs`,
    `crates/opforge-asm/src/fs_uae_smoke.rs`,
    reference artifacts when output changes.
  - Detailed validation: `cargo fmt --all --check`; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; `cargo test -p asm
    examples_match_reference_outputs -- --nocapture`; opt-in FS-UAE native CLI
    smoke when `OPFORGE_FS_UAE_*` environment is available.
  - Full quality gates: `.module main` records module id and line number from
    table state; malformed `.module` returns deterministic native diagnostic.
  - Plan-compliance review evidence: run `plan-compliance-reviewer` before
    commit and record PASS.
  - Commit outcome: one commit making `.module` table-backed while keeping
    `.use` behavior unchanged except where it reads current module context.
  - Detailed definition of done: module summaries are emitted from the native module
    table and malformed module declarations fail before emitter stage.

- [x] Item 3: Add native `.endmodule` depth tracking
  - Validation: focused native CLI host tests, reference gate, format check, and
    opt-in malformed `.endmodule` FS-UAE smoke when available.
  - Definition of done: native CLI tracks open module state across the input
    file and rejects unmatched endings.
  - Source requirement or finding IDs: Spec `.endmodule` behavioral contract and
    EOF boundary cases.
  - Expected files: `examples/motorola68000/amigaos/opforge/opforge_cli.asm`,
    `crates/opforge-asm/src/tests.rs`, reference artifacts when output changes.
  - Detailed validation: `cargo fmt --all --check`; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; `cargo test -p asm
    examples_match_reference_outputs -- --nocapture`; opt-in FS-UAE malformed
    `.endmodule` smoke when available.
  - Full quality gates: underflow and unterminated module diagnostics are
    deterministic; successful `.module`/`.endmodule` source reaches emitter
    stub.
  - Plan-compliance review evidence: run `plan-compliance-reviewer` before
    commit and record PASS.
  - Commit outcome: one commit adding module-depth state and EOF validation.
  - Detailed definition of done: native CLI tracks open module state across the input
    file and rejects unmatched endings.

- [x] Item 4: Parse repeatable native `-M` / `--module-path`
  - Validation: focused native CLI host tests, reference gate, format check, and
    opt-in FS-UAE smoke with two module paths when available.
  - Definition of done: native CLI records repeatable module roots in command
    order after the implicit input root.
  - Source requirement or finding IDs: Spec native `--module-path` processing.
  - Expected files: `examples/motorola68000/amigaos/opforge/opforge_cli.asm`,
    `crates/opforge-asm/src/tests.rs`, `crates/opforge-asm/src/fs_uae_smoke.rs`,
    reference artifacts when output changes.
  - Detailed validation: `cargo fmt --all --check`; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; `cargo test -p asm
    examples_match_reference_outputs -- --nocapture`; opt-in FS-UAE CLI smoke
    with two module paths when available.
  - Full quality gates: `-M DIR` and `--module-path DIR` are no longer reported
    as unsupported; missing value and path table overflow diagnostics are locked.
  - Plan-compliance review evidence: run `plan-compliance-reviewer` before
    commit and record PASS.
  - Commit outcome: one commit adding native CLI module-path storage and
    diagnostics.
  - Detailed definition of done: native CLI records repeatable module roots in command
    order after the implicit input root.

- [ ] Item 5: Replace print-only `.use` recognition with table-backed import
      records
  - Validation: focused native CLI host tests, reference gate, format check, and
    opt-in `.use` success/failure FS-UAE smoke when available.
  - Definition of done: import summaries are emitted from table state and
    invalid import forms fail before emitter stage.
  - Source requirement or finding IDs: Spec `.use` behavioral contract for
    module id, alias, selected items, and wildcard item.
  - Expected files: `examples/motorola68000/amigaos/opforge/opforge_cli.asm`,
    `crates/opforge-asm/src/tests.rs`,
    `crates/opforge-asm/src/fs_uae_smoke.rs`, reference artifacts when output
    changes.
  - Detailed validation: `cargo fmt --all --check`; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; `cargo test -p asm
    examples_match_reference_outputs -- --nocapture`; opt-in FS-UAE success and
    malformed `.use` smoke when available.
  - Full quality gates: `.use math`, `.use math as m`, `.use math(foo as f)`,
    and `.use math (*)` populate parser/import records, with selected-item alias
    semantics explicitly limited to current Rust behavior; malformed forms
    return deterministic diagnostics.
  - Plan-compliance review evidence: run `plan-compliance-reviewer` before
    commit and record PASS.
  - Commit outcome: one commit adding table-backed import records without
    resolving external module files yet.
  - Detailed definition of done: import summaries are emitted from table state and
    invalid import forms fail before emitter stage.

- [ ] Item 6: Add first native module resolution slice
  - Validation: focused native CLI host tests, reference gate, format check, and
    opt-in FS-UAE smoke using one `Work:` module dependency when available.
  - Definition of done: native CLI discovers and loads one external module file
    for a simple `.use` case.
  - Source requirement or finding IDs: Spec module resolution subset and Rust
    source-graph behavior.
  - Expected files: `examples/motorola68000/amigaos/opforge/opforge_cli.asm`,
    `crates/opforge-asm/src/tests.rs`,
    `crates/opforge-asm/src/fs_uae_smoke.rs`, reference artifacts when output
    changes.
  - Detailed validation: `cargo fmt --all --check`; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; `cargo test -p asm
    examples_match_reference_outputs -- --nocapture`; opt-in FS-UAE smoke using
    a `Work:` module dependency when available.
  - Full quality gates: one `.use` dependency resolves through the native module
    path subset; missing module reports a deterministic diagnostic; ambiguous
    module behavior is either implemented or explicitly deferred in the plan.
  - Plan-compliance review evidence: run `plan-compliance-reviewer` before
    commit and record PASS.
  - Commit outcome: one commit adding the smallest working native module file
    lookup path.
  - Detailed definition of done: native CLI can discover and load one external module
    file for a simple `.use` case.

- [ ] Item 7: Connect module/import tables to PRVM parser output
  - Validation: focused native CLI tests, focused PRVM tests if touched,
    reference gate, format check, and opt-in FS-UAE smoke when available.
  - Definition of done: supported module/use processing is fed by parser VM
    output rather than ad-hoc source-line scanning.
  - Source requirement or finding IDs: Spec invariant that text scanning is
    transitional and PRVM/parser output is the long-term input.
  - Expected files: `examples/motorola68000/amigaos/opforge/opforge_cli.asm`,
    PRVM AmigaOS files under `examples/motorola68000/amigaos/prvm/` only if
    the existing parser ABI needs a narrow exported entry, plus tests and
    references.
  - Detailed validation: `cargo fmt --all --check`; `cargo test -p asm
    motorola68020_opforge_native_cli_ -- --nocapture`; focused PRVM tests if
    touched; `cargo test -p asm examples_match_reference_outputs --
    --nocapture`; opt-in FS-UAE native CLI smoke when available.
  - Full quality gates: module/use state can be produced from parser VM output
    for the supported subset; source text scanner remains only as fallback or is
    removed if no longer needed.
  - Plan-compliance review evidence: run `plan-compliance-reviewer` before
    commit and record PASS.
  - Commit outcome: one commit wiring parser VM output into native module/use
    processing.
  - Detailed definition of done: native module/use processing no longer depends on
    ad-hoc source line scanning for supported syntax.

- [ ] Item 8: Lock parity boundaries and update follow-up plan
  - Validation: branch-local spec and plan artifact checks plus focused tests
    for any final expectation changes.
  - Definition of done: implementation status is auditable and remaining work is
    described without claiming unsupported parity.
  - Source requirement or finding IDs: Spec acceptance criteria and open
    questions.
  - Expected files: this plan, optional finding/closure artifact if review
    findings are opened, tests as needed.
  - Detailed validation: `scripts/workflow/check_plan_checkboxes.py
    documentation/plans/opforge-native-amigaos-module-use-port-implementation-plan-v0_1.md`;
    `scripts/workflow/check_spec_artifact.py
    documentation/opForge-native-amigaos-module-use-port-spec-v0_1.md`;
    focused Rust/native tests for any final expectation changes.
  - Full quality gates: open questions are either answered in a follow-up plan
    or explicitly left as deferred work.
  - Plan-compliance review evidence: run `plan-compliance-reviewer` before
    final plan-update commit and record PASS.
  - Commit outcome: one commit updating plan checkboxes and follow-up notes.
  - Detailed definition of done: implementation status is auditable and remaining work is
    described without claiming unsupported parity.

## Milestones

- [ ] Milestone 1: Native CLI owns fixed module/import/path state and diagnostics.
- [ ] Milestone 2: Native CLI records `.module`, `.endmodule`, and `.use` from
      supported input syntax.
- [ ] Milestone 3: Native CLI accepts module path roots and resolves one external
      dependency under FS-UAE.
- [ ] Milestone 4: Supported module/use syntax is fed by PRVM output rather than
      source-line scanning.

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
