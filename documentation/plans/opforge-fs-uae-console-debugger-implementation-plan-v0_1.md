<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# Plan: opForge FS-UAE Console Debugger Implementation v0.1

## Metadata

- Source: User request on 2026-07-15 to plan implementation of FS-UAE's built-in debugger, informed by the supplied “Remote Debugging FS-UAE” conversation and verified against the official FS-UAE console-debugger and keyboard-shortcut documentation.
- Mode: implementation
- Owner: Codex
- AGENTS binding: The active worktree `AGENTS.md` rules remain binding during execution.
- Validation status: Draft; requires `scripts/workflow/run_plan_workflow.sh` and workflow-gate validation before plan-driven implementation begins.

## Goal

Add an opt-in, deterministic diagnostic mode for the existing native FS-UAE
test harness. It must launch stock FS-UAE from a terminal with its built-in
console debugger enabled, inject a bounded command script through a PTY, and
write a machine-readable stop/transcript report for a selected native guest
hang or breakpoint.

The first supported investigation is the native macro completed-definition
hang: stop at the two-pass engine boundary, capture the debugger's CPU state
and stack/disassembly transcript on timeout or breakpoint, and preserve the
artifacts beside the existing `target/fs-uae-*` run directory.

This plan does not add a patched FS-UAE, GDB remote server, `uae-dap`, serial
debug agent, editor integration, source-level relocation maps, or production
assembler semantics. Those are separate follow-on scopes.

## Version Impact

- Affected component(s): `crates/opforge-asm/src/fs_uae_smoke.rs`, native
  FS-UAE debug harnesses/configuration, workflow documentation, and focused
  Rust tests.
- Impact class: none
- Owned contract: the existing FS-UAE test environment gains a separate,
  explicit console-debugger execution mode that produces bounded diagnostic
  artifacts and always cleans up its emulator process tree.
- Rationale: the current file-artifact smoke protocol can distinguish guest
  completion from timeout but cannot identify the 68020 instruction, stack, or
  debugger state at a native hang.

## Constraints

- Use stock FS-UAE's built-in console debugger only. Official documentation
  requires `console_debugger = 1`, a terminal launch, and `Cmd+D` on macOS
  (`Mod+D` generally). Do not represent the console as a stable public API.
- The normal Level D FS-UAE parity path remains non-interactive and unchanged.
  Console-debugger execution is an explicit opt-in localization tool and is
  proof Level E unless a later plan defines a deterministic Level D contract.
- The controller must use a PTY rather than stdin/stdout pipes so FS-UAE sees
  a terminal. It must record raw transcript bytes, sanitized text, launch
  configuration, process IDs, timeout reason, and cleanup outcome.
- Never automate a GUI keyboard shortcut as the primary mechanism. First prove
  whether console commands can be entered through the terminal PTY; if stock
  FS-UAE requires `Cmd+D`, provide an explicitly documented manual-entry
  fallback rather than fragile GUI event injection.
- Native instrumentation remains governed by
  `agents/rules/native-68000-safe-instrumentation.md`; use the approved debug
  contract framework only. Do not add ad-hoc guest printing or mutable-buffer
  probes.
- Each item is one focused commit. Before every native code commit, load
  `agents/rules/native-rust-parity-porting.md`,
  `agents/rules/native-parity-failure-triage.md`,
  `agents/rules/native-68000-safe-instrumentation.md`, and
  `agents/rules/fs-uae.md` as applicable.

## Work Items

- [x] Item 1: Establish and document the stock console-debugger command contract.
  - Source requirement or finding IDs: User request; FS-UAE official
    `console_debugger` and keyboard-shortcut documentation; native macro hang.
  - Expected files: `documentation/fs-uae-console-debugger.md`,
    `crates/opforge-asm/src/fs_uae_smoke.rs` tests only if a parser fixture is
    needed.
  - Full quality gates: `make workflow-gate`; focused Rust unit tests for
    transcript parsing; no emulator launch required for this item.
  - Plan-compliance review evidence: Single-agent `plan-quality-reviewer` PASS
    and `plan-compliance-reviewer` PASS for the three-file documentation slice
    on 2026-07-15.
  - Commit outcome: Documentation-only commit records the
    tested command prompt, command vocabulary, stop markers, and known version
    limits for the installed `/Applications/FS-UAE.app` binary.
  - Definition of done: A checked-in command grammar names exact accepted
    debugger commands for register, stack, disassembly, continue/break, and
    quit; unsupported commands and terminal/GUI prerequisites are explicit.

- [ ] Item 2: Prove or reject stock console activation through a PTY.
  - Source requirement or finding IDs: Item 1 command grammar; official
    `console_debugger` terminal prerequisite; explicit no-GUI-automation
    constraint.
  - Expected files: a narrowly scoped host feasibility helper/test under
    `crates/opforge-asm/src/` or `scripts/workflow/`, focused documentation,
    and no native guest production sources.
  - Full quality gates: focused host test; `scripts/workflow/run_rust_quality_gate.sh`
    if Rust changes; `make workflow-gate` if workflow/docs change.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns PASS
    before commit.
  - Commit outcome: One feasibility-only commit. It records either a stable
    terminal/PTY activation exchange or a reproducible finding that stock
    FS-UAE requires manual `Cmd+D` entry in this environment.
  - Definition of done: The result is explicit and machine-recorded. A failed
    PTY activation does not block manual console use, but it blocks automated
    command injection and redirects later items to transcript capture after
    documented manual entry.

- [ ] Item 3: Add an opt-in PTY FS-UAE console-debugger runner with bounded artifacts.
  - Source requirement or finding IDs: Item 1 debugger contract; existing
    `FsUaeSmokeRun` lifecycle and cleanup contract.
  - Expected files: `crates/opforge-asm/src/fs_uae_smoke.rs`, focused tests in
    `crates/opforge-asm/src/tests.rs`, and a small host-side helper under
    `scripts/workflow/` only if Rust cannot safely own PTY lifecycle.
  - Full quality gates: focused host tests; `scripts/workflow/run_rust_quality_gate.sh`;
    `make workflow-gate` if workflow helper/docs change.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns PASS
    before commit.
  - Commit outcome: One host-tooling commit adding a separate environment gate,
    unique per-run PTY/log paths, timeouts, raw and normalized transcript files,
    and mandatory process-tree cleanup.
  - Definition of done: The runner refuses to run without explicit opt-in,
    launches FS-UAE with a generated config containing `console_debugger = 1`,
    returns a structured result for completed/timeout/manual-intervention, and
    never changes the normal smoke invocation.

- [ ] Item 4: Add a minimal native debug-contract stop harness and command script.
  - Source requirement or finding IDs: Item 1 grammar; approved native debug
    framework; macro completed-definition hang.
  - Expected files: `native/motorola68000/amigaos/test-harnesses/debug/`,
    `native/motorola68000/amigaos/debug/` only if a new stable event ID is
    required, `crates/opforge-asm/src/fs_uae_smoke.rs`, and focused tests.
  - Full quality gates: `scripts/workflow/run_native_68000_format_gate.sh`;
    focused harness assembly test; `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns PASS
    before commit.
  - Commit outcome: One native diagnostic-harness commit that reaches a known
    shared debug-contract event without changing production CLI control flow.
  - Definition of done: The console script can stop on the controlled harness,
    collect registers/stack/disassembly through the verified Item 1 grammar,
    exit cleanly, and write a deterministic transcript fixture. This test
    proves console capture plumbing, not macro parity (Level E).

- [ ] Item 5: Apply the console debugger to the macro hang and publish a reproducible report.
  - Source requirement or finding IDs: Native macro fixture
    `examples/opcore/macro_invocation_native.asm`; Item 2 runner; Item 3
    controlled stop harness.
  - Expected files: focused debug command fixture/config template, a dated
    diagnostic report under `documentation/reviews/` or
    `documentation/plans/slices/`, and only narrowly required test wiring.
  - Full quality gates: focused opt-in FS-UAE console-debugger run;
    `scripts/workflow/run_native_68000_format_gate.sh` if native sources
    change; `scripts/workflow/run_rust_quality_gate.sh`; `make workflow-gate`
    for report/workflow artifact changes.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns PASS
    before commit.
  - Commit outcome: One diagnostic-evidence commit containing no speculative
    semantic fix.
  - Definition of done: The report records FS-UAE version/configuration,
    transcript hash/path, stop reason, PC/SR/D0-D7/A0-A7, stack words,
    disassembly, and the exact source/session fixture. It identifies the first
    non-returning routine or explicitly documents why the stock debugger cannot
    do so.

- [ ] Item 6: Fix the identified native invariant and confirm full macro parity.
  - Source requirement or finding IDs: Item 4 stop report; Rust
    `MacroProcessor` behavior; native macro fixture parity contract.
  - Expected files: smallest native CLI/preprocessor/engine boundary files,
    `crates/opforge-asm/src/tests.rs`, fixture/reference artifacts only when
    output behavior intentionally changes, and a slice metadata file under
    `documentation/plans/slices/`.
  - Full quality gates: focused Level A-C boundary proof; focused Level D
    `native_macro_invocation_fixture_fs_uae`; native formatter gate; Rust
    quality gate; reference refresh gate if fixtures change.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns PASS
    before commit; a finding-closure review passes before the hang is marked
    fixed.
  - Commit outcome: One production-fix commit with a named invariant and no
    debugger-only behavior in the release path.
  - Definition of done: The untouched dotted-call, source-`.cpu 65c02` macro
    fixture exits in FS-UAE and its emitted bytes exactly match the live Rust
    authority. The console debugger is retained only as opt-in tooling.

## Milestones

1. Stock console interaction is documented and reproducible from a terminal.
2. The host runner produces bounded, machine-readable diagnostic artifacts.
3. A controlled guest stop proves the console capture pipeline.
4. The macro hang has a PC/stack-backed root-cause report.
5. The production fix restores full native/Rust parity and the debugger remains
   an opt-in aid rather than a required runtime dependency.

## Blocking Rules

- no commit before all quality gates pass
- `plan-compliance-reviewer` must return `PASS` before commit
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- archive completed plans with `scripts/workflow/archive_completed_plan.sh`
