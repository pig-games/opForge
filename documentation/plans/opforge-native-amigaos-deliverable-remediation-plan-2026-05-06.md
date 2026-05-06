# opForge Native AmigaOS Deliverable Remediation Plan

## Metadata

- Source: `documentation/reviews/opforge-native-amigaos-deliverable-review-2026-05-06.md`
- Mode: remediation
- Owner: implementation agent executing under `AGENTS.md`

## Objective

Remediate the May 6, 2026 native AmigaOS deliverable review findings with
small, ordered, commit-sized slices. The plan keeps the current advancing
native 68020/AmigaOS path intact while tightening the production boundary so
native tokenize, parse, selector/encode, harness layout, and output-mode
behavior align with the intended Rust VM architecture.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- This plan must not become active until `plan-quality-reviewer` or
  `plan-quality-orchestrator` returns `PASS`.
- Scope is limited to findings `RVW-2026-05-06-001` through
  `RVW-2026-05-06-004` and the minimum focused validation needed to close them.
- Execute one work item at a time; do not begin the next item until the current
  item is validated, reviewed by `plan-compliance-reviewer`, committed, and its
  checkbox state is updated.
- When a work item fully closes a finding, do not mark the finding fixed or
  complete the item's checkbox until a closure artifact exists and
  `finding-closure-reviewer` returns `PASS` for that same commit.
- Do not broaden this remediation into a full native Hunk writer, a macro or
  module-system redesign, or unrelated cleanup outside the files required by
  the listed findings.
- Harness relocation work must update any Rust tests, FS-UAE smoke wiring, and
  documentation references in the same slice so moved files do not leave stale
  paths behind.
- `--hunk` remains a truthful not-implemented surface until a dedicated native
  Hunk writer is explicitly planned and delivered as separate work.
- Do not install, import, add, recommend, vendor, execute, or otherwise touch
  `litellm`.

## Version Impact

- Affected component(s): native AmigaOS opForge CLI, native tkpkg service,
  native PRVM and planned native opasm runtime modules, native harness layout,
  and native output-mode handling.
- Impact class: internal architecture and native deliverable readiness.
- Owned contract: native host/runtime layering for package-backed opForge
  targets, including truthful CLI surface area and deliverable-tree boundaries.
- Rationale: the source review does not identify an external Rust CLI contract
  change, but it does identify service-boundary, layering, and deliverable-shape
  mismatches that should be corrected before the native tree is treated as a
  stable deliverable surface.

## Work Items

- [x] Item 1: Route native parse through tkpkg service and remove the direct CLI shortcut
  - Source requirement or finding IDs: `RVW-2026-05-06-001`; expected to fully
    close the finding.
  - Expected files: `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm`,
    `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm`, and only the
    smallest supporting native PRVM or ABI file needed to keep the service
    envelope coherent; focused coverage in `crates/opforge-asm/src/tests.rs`.
  - Full quality gates: focused `cargo test -p asm motorola68020_tkpkg_ --
    --nocapture`, focused `cargo test -p asm motorola68020_prvm_line_router_ --
    --nocapture`, focused `cargo test -p asm motorola68020_opforge_native_cli_
    -- --nocapture`, plus `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run `Plan Compliance
    Reviewer` with `AGENTS.md`, this plan, the Item 1 slice summary, changed
    files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that makes `ENTRY_ORD_PARSE_LINE` a live
    tkpkg service path and removes direct CLI use of `prvm_route_line_68000`
    from the production parse flow.
  - Definition of done: parse joins tokenize and encode behind the same native
    tkpkg service boundary, the CLI uses the parse service envelope instead of a
    private PRVM shortcut, and focused tests prove the service dispatcher owns
    the live parse entrypoint.

- [x] Item 2: Introduce a native opasm runtime stage for selector request construction
  - Source requirement or finding IDs: `RVW-2026-05-06-002`; expected to
    partially close the finding by moving selector request construction and
    supported-subset policy for the initial fixed native subset out of the CLI;
    package-metadata-driven generalization remains deferred to Item 3.
  - Expected files: a new native runtime module subtree under
    `native/motorola68000/amigaos/` for opasm-owned selector staging,
    `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm` only for the
    narrow integration hook that calls the new runtime stage and pre-resolves
    known label operands to fixed hex text before staging, and focused coverage
    in `crates/opforge-asm/src/tests.rs` and `crates/opforge-asm/src/fs_uae_smoke.rs`.
  - Full quality gates: focused `cargo test -p asm motorola68020_opforge_native_cli_
    -- --nocapture`, focused `cargo test -p asm motorola68020_prvm_ --
    --nocapture`, focused `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture`, plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run `Plan Compliance
    Reviewer` with `AGENTS.md`, this plan, the Item 2 slice summary, changed
    files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that adds a native opasm runtime stage
    consuming CLI-staged statement and operand text for the initial supported
    subset to build selector and encode requests outside the CLI; package
    metadata consumption remains part of the later full cutover.
  - Definition of done: selector request construction, mnemonic acceptance
    policy, and local PC-size advancement policy for the fixed initial subset
    move into an opasm-owned native runtime surface, while the CLI retains only
    boundary duties needed for this slice such as known-label pre-resolution.

- [x] Item 3: Cut the CLI over to the native runtime stage and delete CLI-owned selector logic
  - Source requirement or finding IDs: `RVW-2026-05-06-002`; expected to fully
    close the finding.
  - Expected files: `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm`,
    the Item 2 native opasm runtime module files, and focused coverage in
    `crates/opforge-asm/src/tests.rs` plus `crates/opforge-asm/src/fs_uae_smoke.rs`
    if the end-to-end smoke path needs updated assertions.
  - Full quality gates: focused `cargo test -p asm motorola68020_opforge_native_cli_
    -- --nocapture`, focused `cargo test -p asm motorola68020_tkpkg_ --
    --nocapture`, opt-in `cargo test -p asm external_fs_uae_hunk_smoke --
    --nocapture` when the native CLI smoke surface changes, plus
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run `Plan Compliance
    Reviewer` with `AGENTS.md`, this plan, the Item 3 slice summary, changed
    files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that makes the CLI pass parsed statement
    records into the native runtime stage and removes the hand-coded `lda`/
    `sta`/`jmp` acceptance, addressing-mode selection, operand payload shaping,
    and PC advancement logic from the CLI.
  - Definition of done: the CLI is only the host and orchestration layer for
    the supported subset, package-backed selector and encoder decisions occur in
    the native runtime stage, and focused coverage shows the CLI no longer acts
    as a second selector implementation.

- [x] Item 4: Move non-deliverable harnesses into a clearly named native test-harness subtree
  - Source requirement or finding IDs: `RVW-2026-05-06-003`; expected to fully
    close the finding.
  - Expected files: the named non-deliverable native entrypoints from the
    review (`tkpkg_debug_cli.asm`, `prvm_debug_cli.asm`, `prvm_smoke.asm`,
    `prvm_line_iterator_smoke.asm`, and `tokvm_test_input.asm`) relocated under
    a dedicated `native/motorola68000/amigaos/test-harnesses/` subtree,
    `native/README.md`, and any required path updates in
    `crates/opforge-asm/src/tests.rs` and `crates/opforge-asm/src/fs_uae_smoke.rs`.
  - Full quality gates: focused `cargo test -p asm motorola68020_prvm_ --
    --nocapture`, focused `cargo test -p asm motorola68020_tkpkg_ --
    --nocapture`, focused `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture`, opt-in
    `cargo test -p asm external_fs_uae_hunk_smoke -- --nocapture` if the smoke
    launcher path list changes, plus `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run `Plan Compliance
    Reviewer` with `AGENTS.md`, this plan, the Item 4 slice summary, changed
    files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that leaves deliverable runtime modules
    under the production subtrees and moves the named smoke, debug, and sample
    entrypoints under a clearly non-deliverable native harness location.
  - Definition of done: the native tree visibly separates deliverable runtime
    modules from debug and smoke tooling, and all tests, docs, and FS-UAE smoke
    wiring resolve the new harness paths without stale references.

- [ ] Item 5: Split native output format state and make `--hunk` deterministic and honest
  - Source requirement or finding IDs: `RVW-2026-05-06-004`; expected to fully
    close the finding.
  - Expected files: `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm`,
    focused coverage in `crates/opforge-asm/src/tests.rs`, and
    `crates/opforge-asm/src/fs_uae_smoke.rs` only if the native CLI smoke
    assertions need to distinguish `--bin` and `--hunk` behavior.
  - Full quality gates: focused `cargo test -p asm motorola68020_opforge_native_cli_surface_locks_rust_subset_flag_names -- --nocapture`, focused
    `cargo test -p asm motorola68020_opforge_native_cli_6502_small_assembly_contract_matches_rust_vm_bytes -- --nocapture`, focused `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`, opt-in `cargo test -p asm external_fs_uae_hunk_smoke -- --nocapture` when the native CLI smoke contract changes, plus `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run `Plan Compliance
    Reviewer` with `AGENTS.md`, this plan, the Item 5 slice summary, changed
    files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that introduces explicit native output
    format state, routes only `--bin` to the flat-byte writer, and makes
    `--hunk` fail with a deterministic not-implemented diagnostic.
  - Definition of done: `--bin` and `--hunk` no longer share one implicit path
    variable, `--bin` preserves the current flat-output behavior for the
    supported slice, and `--hunk` cannot silently emit flat bytes.

## Milestones

- [x] Milestone 1: The native tkpkg service boundary consistently owns
  tokenize, parse, and encode entrypoints.
- [x] Milestone 2: CLI-owned selector behavior has been moved into a native
  opasm runtime stage and the CLI no longer duplicates selector policy.
- [ ] Milestone 3: The native tree clearly separates deliverable runtime
  modules from harnesses, and the output-mode contract is truthful for `--bin`
  and `--hunk`.
- [ ] Milestone 4: All four review findings have commit-ready remediation and
  closure evidence.

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no plan execution starts until the plan-quality reviewer returns `PASS`
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- no finding-fixed claim or completed finding checkbox before a closure artifact
  exists and `finding-closure-reviewer` returns `PASS` for the same commit
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping