# AmigaOS Assembler Review Remediation Plan

## Metadata

- Source: `dev-docs/reviews/opforge_amigaos_asm_review_2026-05-04.md`
- Mode: remediation
- Owner: implementation agent executing under `AGENTS.md`

## Objective

Remediate the six AmigaOS assembler review findings from the May 4, 2026
review report with small, ordered, commit-sized slices. The plan prioritizes
package trust-boundary fixes before user-visible correctness fixes, because the
package walkers and selected tokenizer VM reader can otherwise read or write
past intended native runtime bounds under malformed input.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Scope is limited to the six findings in the source review report and the
  minimum validation needed to prove each remediation.
- Execute one work item at a time; do not begin the next item until the current
  item is validated, reviewed by `plan-compliance-reviewer`, committed, and its
  checkbox state is updated.
- Do not update release notes unless a separate release-bearing task provides
  version-impact evidence and explicitly requests it.
- Do not install, import, add, recommend, vendor, execute, or otherwise touch
  `litellm`.

## Version Impact

- Affected component(s): tkpkg native runtime example, tokvm AmigaOS CLI
  harness, PRVM line iterator, and opforge native CLI example.
- Impact class: Behavioral defect and malformed-input safety gap in example
  binaries.
- Owned contract: Native AmigaOS example ABIs for `load_package`,
  `set_pipeline`, `tokenize_line`, PRVM iteration, and CLI status reporting.
- Rationale: The source review identified paths that can read or write past
  intended native buffers under malformed package or input data, plus
  user-visible accounting and reporting defects on valid edge cases.

## Work Items

- [x] Item 1: Bound tkpkg chunk walkers during pipeline resolution
  - Source requirement or finding IDs: RVW-2026-05-04-006; expected to fully
    close the finding.
  - Expected files: `examples/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm`,
    `examples/motorola68000/amigaos/tkpkg/tkpkg_token_policy.asm`, and the
    smallest focused native tkpkg regression test or harness fixture needed to
    exercise malformed CPUS, FAMS, DIAL, TOKS, and TKVM chunk records.
  - Full quality gates: focused malformed-package tests proving `set_pipeline`
    rejects chunk-internal record counts, string lengths, optional string lists,
    and entry skips that exceed the selected chunk; affected AmigaOS examples
    still assemble; then `scripts/workflow/run_rust_quality_gate.sh` and
    `make workflow-gate` if workflow artifacts were updated.
  - Plan-compliance review evidence: before commit, run `Plan Compliance
    Reviewer` with `AGENTS.md`, this plan, Item 1 summary, changed files, and
    validation output; require `PASS`.
  - Commit outcome: exactly one commit that carries a chunk-end bound through
    pipeline and token-policy traversal helpers and returns a deterministic
    package/runtime error before any helper advances beyond that bound.
  - Definition of done: malformed package chunk metadata cannot make
    `set_pipeline` read outside the selected chunk, valid package selection
    still succeeds, focused regressions pass, and the full quality gates pass.
  - Completion evidence: `cargo test -p asm motorola68020_tkpkg -- --nocapture`
    passed with 28 tests; `cargo test -p asm
    external_fs_uae_tkpkg_native_rejects_selected_chunk_bounds_during_set_pipeline -- --nocapture`
    passed with the configured FS-UAE opt-in skip; `scripts/workflow/run_rust_quality_gate.sh`
    passed; `make workflow-gate` passed.

- [x] Item 2: Reject over-capacity active pipeline identifiers
  - Source requirement or finding IDs: RVW-2026-05-04-001; expected to fully
    close the finding.
  - Expected files: `examples/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm`,
    possibly `examples/motorola68000/amigaos/tkpkg/tkpkg_buffers.asm` only if a
    named error constant or capacity helper is required, and the focused native
    tkpkg regression test or fixture for long resolved CPU, dialect, and family
    identifiers.
  - Full quality gates: focused malformed-package tests proving resolved
    identifiers of length `PIPELINE_ID_BUFFER_CAPACITY` or greater are rejected
    before copying while 31-byte identifiers still terminate correctly; affected
    AmigaOS examples still assemble; then `scripts/workflow/run_rust_quality_gate.sh`
    and `make workflow-gate` if workflow artifacts were updated.
  - Plan-compliance review evidence: before commit, run `Plan Compliance
    Reviewer` with `AGENTS.md`, this plan, Item 2 summary, changed files, and
    validation output; require `PASS`.
  - Commit outcome: exactly one commit that checks locator lengths before
    writing active CPU, dialect, and family buffers and reports a deterministic
    runtime/package error for over-capacity identifiers.
  - Definition of done: active selection buffer writes cannot overrun the
    fixed 32-byte buffers, valid maximum-length identifiers remain supported,
    focused regressions pass, and the full quality gates pass.
  - Completion evidence: `cargo test -p asm motorola68020_tkpkg -- --nocapture`
    passed with 30 tests; `cargo test -p asm
    external_fs_uae_tkpkg_native_rejects_over_capacity_active_family_identifier -- --nocapture`
    passed with the configured FS-UAE opt-in skip; `scripts/workflow/run_rust_quality_gate.sh`
    passed after the intentional reference refresh; `make workflow-gate` passed.

- [x] Item 3: Enforce selected TKVM record bounds while decoding programs
  - Source requirement or finding IDs: RVW-2026-05-04-002; expected to fully
    close the finding.
  - Expected files: `examples/motorola68000/amigaos/tkpkg/tkpkg_tokenizer_vm.asm`,
    `examples/motorola68000/amigaos/tkpkg/tkpkg_token_policy.asm`, and the
    focused native tkpkg tokenizer regression test or fixture for forged
    selected TKVM record-internal lengths.
  - Full quality gates: focused malformed-package tests proving `tokenize_line`
    rejects forged selected TKVM string, state-table, diagnostic-code, and
    bytecode lengths that cross the selected record end; valid tokenizer VM
    packages still tokenize known example lines; affected AmigaOS examples still
    assemble; then `scripts/workflow/run_rust_quality_gate.sh` and
    `make workflow-gate` if workflow artifacts were updated.
  - Plan-compliance review evidence: before commit, run `Plan Compliance
    Reviewer` with `AGENTS.md`, this plan, Item 3 summary, changed files, and
    validation output; require `PASS`.
  - Commit outcome: exactly one commit that carries a record-end pointer through
    every TKVM decode helper and fails before reading or skipping beyond that
    record.
  - Definition of done: malformed selected TKVM program records cannot make
    `tokenize_line` read unrelated package or BSS bytes, valid tokenization
    remains unchanged, focused regressions pass, and the full quality gates
    pass.
  - Completion evidence: `cargo test -p asm motorola68020_tkpkg -- --nocapture`
    passed with 31 tests; `cargo test -p asm
    external_fs_uae_tkpkg_native_rejects_truncated_conditional_jump_tokenizer_program -- --nocapture`
    initially passed by taking the opt-in skip path; after configuring the
    documented FS-UAE binary/template, the real emulator exposed an older native
    `set_pipeline` token-policy locator bug (`OTR003: missing tokenizer policy`)
    that also reproduced from a clean detached `HEAD`; preserving the incoming
    owner locator while clearing `pendingTokenPolicy*` fixed that blocker;
    `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae'
    OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae'
    OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path
    crates/opforge-asm/Cargo.toml external_fs_uae_tkpkg_native_rejects_truncated_conditional_jump_tokenizer_program -- --nocapture`
    passed under real FS-UAE; `opForge_UPDATE_REFERENCE=1 cargo test -p asm
    examples_match_reference_outputs -- --nocapture` passed for the intentional
    reference refresh; `scripts/workflow/run_rust_quality_gate.sh` passed;
    `make workflow-gate` passed.

- [x] Item 4: Return logical PRVM line counts independently of absolute line numbers
  - Source requirement or finding IDs: RVW-2026-05-04-003; expected to fully
    close the finding.
  - Expected files: `examples/motorola68000/amigaos/prvm/prvm_line_iterator.asm`,
    `examples/motorola68000/amigaos/prvm/prvm_line_iterator_smoke.asm` or the
    smallest equivalent focused PRVM iterator regression fixture.
  - Full quality gates: focused PRVM iterator test with
    `ITER_FRAME_START_LINE_NUM` greater than 1 proving `D3` reports total
    logical lines on both success and fail-fast paths while `D2` keeps the
    absolute failing line; affected AmigaOS examples still assemble; then
    `scripts/workflow/run_rust_quality_gate.sh` and `make workflow-gate` if
    workflow artifacts were updated.
  - Plan-compliance review evidence: before commit, run `Plan Compliance
    Reviewer` with `AGENTS.md`, this plan, Item 4 summary, changed files, and
    validation output; require `PASS`.
  - Commit outcome: exactly one commit that introduces separate logical-line
    accounting for `D3` and reserves `D6` for absolute routed line numbers.
  - Definition of done: non-1 start-line iteration returns correct logical line
    totals without changing routed line numbers, focused regressions pass, and
    the full quality gates pass.
  - Completion evidence: `cargo test --manifest-path crates/opforge-asm/Cargo.toml
    motorola68020_prvm_line_iterator -- --nocapture` passed with 7 tests;
    `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae'
    OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae'
    OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path
    crates/opforge-asm/Cargo.toml external_fs_uae_hunk_smoke -- --nocapture`
    passed under real FS-UAE and completed `prvm_line_iterator_smoke`;
    `opForge_UPDATE_REFERENCE=1 cargo test --manifest-path crates/opforge-asm/Cargo.toml
    examples_match_reference_outputs -- --nocapture` passed with no checked-in
    reference changes for this slice.

- [x] Item 5: Reject oversized external package files before `load_package`
  - Source requirement or finding IDs: RVW-2026-05-04-004; expected to fully
    close the finding.
  - Expected files: `examples/motorola68000/amigaos/opforge/opforge_cli.asm`
    and the smallest focused native CLI regression test or fixture that feeds an
    external package larger than `PACKAGE_STORAGE_CAPACITY`.
  - Full quality gates: focused AmigaOS CLI test proving a
    `PACKAGE_STORAGE_CAPACITY + 1` external package fails with a dedicated
    package-too-large path before `load_package`, while an exact-capacity or
    smaller valid package still reaches the package loader; affected AmigaOS
    examples still assemble; then `scripts/workflow/run_rust_quality_gate.sh`
    and `make workflow-gate` if workflow artifacts were updated.
  - Plan-compliance review evidence: before commit, run `Plan Compliance
    Reviewer` with `AGENTS.md`, this plan, Item 5 summary, changed files, and
    validation output; require `PASS`.
  - Commit outcome: exactly one commit that adds a one-byte overflow probe after
    the bounded external package read and reports a deterministic host-side
    oversized-package error before dispatching `ENTRY_ORD_LOAD_PACKAGE`.
  - Definition of done: oversized external packages are rejected early with a
    clear CLI error, non-oversized external packages keep the previous behavior,
    focused regressions pass, and the full quality gates pass.
  - Completion evidence: `cargo test --manifest-path crates/opforge-asm/Cargo.toml
    motorola68020_opforge_native_cli -- --nocapture` passed with 2 tests;
    `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae'
    OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae'
    OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path
    crates/opforge-asm/Cargo.toml
    external_fs_uae_opforge_native_cli_failure_paths_report_diagnostics --
    --nocapture` passed under real FS-UAE with the
    `OPFORGE_FS_UAE_NATIVE_CLI_PACKAGE_TOO_LARGE` case expecting
    `ERROR OPC-NCLI019: opasm package exceeds native package storage capacity`;
    `opForge_UPDATE_REFERENCE=1 cargo test --manifest-path crates/opforge-asm/Cargo.toml
    examples_match_reference_outputs -- --nocapture` passed and refreshed the
    native CLI reference outputs. A separate real-FS-UAE valid native CLI stub
    run reached `STATUS tokenizer-ok` with the normal 3973-byte package, then
    failed in the existing module/use parser success assertion; that parser
    follow-on remains outside RVW-2026-05-04-004.

- [x] Item 6: Preserve signed status values in tokvm harness reports
  - Source requirement or finding IDs: RVW-2026-05-04-005; expected to fully
    close the finding.
  - Expected files: `examples/motorola68000/amigaos/tokvm/tokvm_cli_harness.asm`
    and the focused native tokvm harness regression test or fixture for negative
    `HARNESS_STATUS_*` formatting.
  - Full quality gates: focused tokvm harness report test proving at least one
    negative host failure status renders as the original decimal value instead
    of `-0`, plus a nonnegative status formatting sanity check; affected
    AmigaOS examples still assemble; then `scripts/workflow/run_rust_quality_gate.sh`
    and `make workflow-gate` if workflow artifacts were updated.
  - Plan-compliance review evidence: before commit, run `Plan Compliance
    Reviewer` with `AGENTS.md`, this plan, Item 6 summary, changed files, and
    validation output; require `PASS`.
  - Commit outcome: exactly one commit that preserves the original signed value
    across the minus-sign write and passes its negated magnitude to the existing
    unsigned formatter.
  - Definition of done: negative harness statuses render with their true
    numeric value, nonnegative statuses remain unchanged, focused regressions
    pass, and the full quality gates pass.
  - Completion evidence: `cargo test --manifest-path crates/opforge-asm/Cargo.toml
    motorola68020_tokvm -- --nocapture` passed with 28 tests, including
    `motorola68020_tokvm_interpreter_preserves_signed_status_before_minus_write`;
    `opForge_UPDATE_REFERENCE=1 cargo test --manifest-path crates/opforge-asm/Cargo.toml
    examples_match_reference_outputs -- --nocapture` passed and refreshed the
    tokvm interpreter reference outputs; `OPFORGE_FS_UAE_SMOKE=1
    OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae'
    OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae'
    OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path
    crates/opforge-asm/Cargo.toml external_fs_uae_hunk_smoke -- --nocapture`
    passed under real FS-UAE and completed `helloworld`, `writefile`,
    `tkpkg_debug_cli`, `prvm_smoke`, and `prvm_line_iterator_smoke`.

## Milestones

- [ ] Milestone 1: tkpkg package-boundary remediations complete and committed
  for RVW-2026-05-04-006, RVW-2026-05-04-001, and RVW-2026-05-04-002.
- [ ] Milestone 2: PRVM line-iterator accounting remediation complete and
  committed for RVW-2026-05-04-003.
- [ ] Milestone 3: native CLI external-package overflow remediation complete
  and committed for RVW-2026-05-04-004.
- [ ] Milestone 4: tokvm harness signed-status remediation complete and
  committed for RVW-2026-05-04-005.
- [ ] Milestone 5: all six findings have closure evidence ready for the
  finding-closure workflow.

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no plan execution starts until the plan-quality reviewer returns `PASS`
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping