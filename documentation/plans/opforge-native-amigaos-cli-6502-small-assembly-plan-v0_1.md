# opForge Native AmigaOS CLI 6502 Small Assembly Plan v0.1

## Metadata

- Source: User request on 2026-05-05 to plan how the native 68020 AmigaOS CLI can assemble small programs, starting with 6502, while following the Rust engine and VM interaction architecture.
- Mode: implementation
- Owner: opForge implementation agent

## Source Artifact And Requirement Mapping

- Source artifact type: explicitly scoped user instruction dated 2026-05-05.
- Source artifact scope: plan how the native 68020 AmigaOS CLI can assemble a
  small program, starting with 6502, while following the Rust engine and VM
  interaction architecture.
- Source requirement IDs used by this plan:
  - `SR-CLI-6502`: deliver a native AmigaOS CLI path that can assemble the
    narrow 6502 single-file smoke program end to end.
  - `SR-RUST-PARITY`: keep native CLI bytes, flag surface, and diagnostic
    structure aligned with the overlapping Rust CLI behavior for the supported
    subset.
  - `SR-AGENTS-COMPLIANCE`: keep the active worktree `AGENTS.md` rules, plan
    quality gate, plan-compliance gate, and commit sequencing rules binding
    throughout execution.
  - `SR-ENGINE-STATE`: add only the minimal native assembly session state needed
    for pass control, PC/origin tracking, symbols, rows, and image bytes.
  - `SR-VM-ENVELOPE`: stabilize the native VM call envelope so the CLI uses the
    same service shape across package load, tokenize, parse, encode, and error
    reporting flows.
  - `SR-PRVM-ROWS`: preserve parser VM ownership of statement structure by
    promoting PRVM output into native emitter-ready rows instead of re-parsing in
    the CLI.
  - `SR-NO-EXPR-IN-PARSER`: keep expression parsing and evaluation out of
    PRVM/opasm statement parsing and out of CLI glue.
  - `SR-EXVM-OPCORE`: route generic math expression parsing and evaluation
    through the native opcore/EXVM bridge.
  - `SR-NO-CPU-IN-OPCORE`: keep CPU-specific operand semantics and encoding out
    of opcore.
  - `SR-OPASM-SELECTOR`: use the opasm selector layer for 6502 candidate
    selection from parsed operands plus evaluated expressions.
  - `SR-6502-FIRST`: keep the first implementation slice host-first, CPU-first,
    and limited to the 6502 smoke-program subset.
  - `SR-OPASM-ENCODER`: use the package-backed opasm encoder layer to emit final
    6502 bytes into the native session image.
  - `SR-ENGINE-PASSES`: add only the minimal native two-pass engine needed for
    the smoke path.
  - `SR-SYMBOLS`: support the symbol behavior required for the narrow slice,
    including the local forward label used by the smoke program.
  - `SR-OUTPUT-BIN`: produce deterministic flat binary output first, before any
    broader output formats.
  - `SR-CLI-CONTRACT`: keep CLI argument ownership, IO ownership, exit codes,
    and overlapping diagnostics/output behavior structurally aligned with the
    Rust CLI.
  - `SR-FS-UAE-GATE`: make the supported native path repeatable under FS-UAE as
    the end-to-end validation gate.
  - `SR-DETERMINISTIC-DIAGNOSTICS`: require stable non-zero failures and
    deterministic diagnostics for the selected negative paths.

## Objective

Make the native 68020 AmigaOS `opforge` CLI assemble a small single-file 6502
program end to end. The first successful target is deliberately narrow:

```asm
        .cpu 6502
        .org $0800
start:  lda #$42
        sta $0200
done:   jmp done
```

The native CLI should produce deterministic flat output bytes for that class of
program and report diagnostics in the same structural style as the Rust CLI.
The implementation must follow the existing Rust architecture:

- the CLI owns arguments, file IO, package loading, engine orchestration,
  diagnostics, and output writing
- the tokenizer VM owns tokenization
- the parser VM / opasm VM owns statement and operand-shape parsing
- the opcore expression VM owns generic math expression parse and evaluation
- the opasm selector and encoder layer owns CPU/family-specific instruction
  selection and byte encoding
- output helpers consume an assembled image rather than reaching back into parse
  or encode internals

The plan starts with 6502 because the Rust side already has a package-backed
`Native6502Harness`, `HierarchyExecutionModel` parser/encoder surfaces,
mos6502 selector bridging, and flat binary output helpers. The native AmigaOS
side currently has package loading, tokenizer integration, PRVM line routing,
and statement record capture, but still stops at `OPC-NCLI009: native emitter VM
not implemented`.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- The plan does not become active until the plan-quality reviewer returns
  `PASS`.
- Work proceeds one checked work item at a time.
- Each work item or phase ends in a new commit before the next item starts.
- The first implementation path is host-first, CPU-specific, and small-program
  only: native 68020 AmigaOS CLI assembling flat 6502 output.
- Do not move CPU-specific operand or encoding semantics into opcore. Opcore
  remains generic math expression parsing and evaluation only.
- Do not move expression math into the parser VM or CLI. PRVM/opasm may identify
  expression spans and operand shapes, but EXVM/opcore must parse and evaluate
  generic expressions.
- Do not add a broad assembler rewrite, module-system redesign, LSP/fixit work,
  macro expansion, relocation support, or multi-output support in this plan.
- Existing dirty worktree changes not made by the executing agent must be
  preserved and worked with, not reverted.

## Version Impact

- Affected component(s): native 68020 AmigaOS CLI, native tkpkg/PRVM/opasm/EXVM
  integration, mos6502 package-backed selector/encoder path, CLI output path,
  FS-UAE native smoke tests, Rust parity fixtures.
- Impact class: feature implementation with externally visible native CLI
  behavior.
- Owned contract: the native CLI subset should keep the Rust CLI argument and
  diagnostic structure where the subset overlaps, and VM interactions should
  mirror the Rust `HierarchyExecutionModel` order.
- Rationale: native tokenization and parsing are already wired; the next useful
  product step is assembling a tiny program instead of only proving tokenizer
  and parser stages.

## Architecture Map

The Rust implementation currently provides the model the native path should
mirror:

- `crates/opforge-engine/src/lib.rs` drives source preparation, pass control,
  output-plan resolution, pass 1, pass 2, and image output.
- `crates/opforge-vm/src/vm_opasm.rs` exposes tokenizer, parser, instruction
  encoding, and output helper surfaces such as
  `parse_portable_line_for_assembler`, `encode_instruction_from_exprs`, and
  `build_bin_output_payload`.
- `crates/opforge-vm/src/execution_model.rs` and its bridge modules validate
  parser contracts, resolve parser VM programs, resolve expression contracts,
  call expression parsing/evaluation, select CPU candidates, and encode bytes.
- `crates/opforge-vm/src/native6502.rs` already models the desired native ABI
  shape with init, load-package, set-pipeline, tokenize-line, parse-line,
  encode-instruction, and last-error entrypoints.
- `crates/opforge-asm/src/asmline_instruction.rs` shows the Rust assembler
  instruction path: parsed instruction plus expression operands enter the VM
  selector/encoder bridge, which returns concrete bytes.
- `examples/motorola68000/amigaos/opforge/opforge_cli.asm` is the native CLI
  host. It should remain the coordinator, not the place where 6502 operand
  semantics are recreated.

Native processor ownership should be:

- CLI engine processor: argv parsing, package path, source files, include/module
  path routing, pass loop, symbol/image state, output file writing, exit codes.
- TKVM processor: package-backed line tokenization for selected CPU/dialect.
- PRVM/opasm statement processor: label, directive, mnemonic, operand span, and
  expression-request records.
- EXVM/opcore expression processor: generic math expression parse and concrete
  evaluation with callbacks for current address, symbol lookup, and pass mode.
- opasm selector processor: 6502 operand classification and candidate
  selection, owned by mos6502 package data or its native generated equivalent.
- opasm encoder processor: final opcode and operand-byte emission for selected
  6502 candidate.
- output processor: flat image to `--bin` first, then later hex/listing/hunk
  support as separate slices.

## Work Items

- [x] Item 1: Lock the native small-assembly target and CLI subset
  - Source requirement or finding IDs: SR-CLI-6502, SR-RUST-PARITY, SR-AGENTS-COMPLIANCE
  - Expected files: `crates/opforge-asm/src/tests.rs`, `examples/motorola68000/amigaos/opforge/opforge_cli.asm`, `examples/mos6502/6502_native_cli_smoke.asm` or an equivalent focused fixture, native CLI reference outputs if surface output changes.
  - Full quality gates: focused Rust parity test for the tiny 6502 program; `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`; `cargo test -p vm native6502_ -- --nocapture`; `cargo fmt --all --check`; `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` must return `PASS` for Item 1 before commit.
  - Commit outcome: one commit that defines the exact native small-assembly smoke contract and keeps current behavior green.
  - Definition of done: the first supported source form, CLI flags, expected bytes, expected diagnostics, and Rust reference path are locked without adding native assembly behavior yet.

- [x] Item 2: Add native assembly session state and flat image buffers
  - Source requirement or finding IDs: SR-ENGINE-STATE, SR-CLI-6502
  - Expected files: `examples/motorola68000/amigaos/opforge/opforge_cli.asm`, `crates/opforge-asm/src/tests.rs`, native CLI reference outputs if changed.
  - Full quality gates: `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`; `cargo test -p asm examples_match_reference_outputs -- --nocapture`; native CLI FS-UAE smoke when the assembled binary changes; `cargo fmt --all --check`; `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` must return `PASS` for Item 2 before commit.
  - Commit outcome: one commit that gives the native CLI pass-local state for CPU id, origin/current PC, source line records, statement records, label table seed, and flat image bytes.
  - Definition of done: the native CLI can initialize, clear, and report an assembly session without changing tokenizer/parser results or pretending to encode instructions.

- [x] Item 3: Stabilize the native VM service envelope for parse and encode
  - Source requirement or finding IDs: SR-VM-ENVELOPE, SR-RUST-PARITY
  - Expected files: `examples/motorola68000/amigaos/opforge/opforge_cli.asm`, `examples/motorola68000/amigaos/tkpkg/*` or shared native ABI includes if needed, `crates/opforge-vm/src/native6502_abi.rs`, `crates/opforge-vm/src/native6502.rs`, `crates/opforge-asm/src/tests.rs`.
  - Full quality gates: `cargo test -p vm native6502_ -- --nocapture`; `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`; `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`; `cargo fmt --all --check`; `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` must return `PASS` for Item 3 before commit.
  - Commit outcome: one commit that documents and tests the native service envelope shared by load-package, set-pipeline, tokenize, parse, encode, and last-error flows.
  - Definition of done: the native CLI has a stable call shape that can carry parser records and encoder requests without inventing a separate CLI-only ABI.

- [x] Item 4: Promote PRVM statement records into emitter-ready native rows
  - Source requirement or finding IDs: SR-PRVM-ROWS, SR-NO-EXPR-IN-PARSER
  - Expected files: `examples/motorola68000/amigaos/opforge/opforge_cli.asm`, `crates/opforge-asm/src/tests.rs`, native CLI reference outputs if the source-surface lock changes.
  - Full quality gates: `cargo test -p vm parser_vm_v2_parity -- --nocapture`; `cargo test -p asm motorola68020_prvm_ -- --nocapture`; `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`; `cargo test -p asm examples_match_reference_outputs -- --nocapture`; `cargo fmt --all --check`; `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` must return `PASS` for Item 4 before commit.
  - Commit outcome: one commit that converts PRVM output into native rows for label, directive kind, mnemonic span, operand spans, and expression-request spans.
  - Definition of done: the native CLI can enumerate parsed rows for the 6502 smoke source in pass order, while expression contents remain spans/requests rather than host-parsed math.

- [x] Item 5: Add the native opcore expression request and evaluation bridge
  - Source requirement or finding IDs: SR-EXVM-OPCORE, SR-NO-CPU-IN-OPCORE
  - Expected files: `examples/motorola68000/amigaos/opforge/opforge_cli.asm`, native opcore/EXVM AmigaOS runtime files if present or newly introduced, `crates/opforge-vm/src/vm_opcore.rs`, `crates/opforge-vm/src/runtime_tests.rs`, `crates/opforge-asm/src/tests.rs`.
  - Full quality gates: focused opcore expression VM tests for `$0800`, `$0200`, decimal, unary, and symbol/current-PC callbacks; `cargo test -p vm vm_runtime_mos6502_ -- --nocapture`; `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`; FS-UAE smoke for expression evaluation once native code is touched; `cargo fmt --all --check`; `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` must return `PASS` for Item 5 before commit.
  - Commit outcome: one commit that lets native code submit expression spans to opcore and receive concrete values or deterministic unresolved diagnostics.
  - Definition of done: `.org $0800`, `#$42`, `$0200`, and `done` can be represented as expression requests, with concrete literals evaluated by opcore and unresolved symbols deferred or diagnosed by pass mode.

- [x] Item 6: Add the native 6502 selector bridge in opasm VM
  - Source requirement or finding IDs: SR-OPASM-SELECTOR, SR-6502-FIRST
  - Expected files: `examples/motorola68000/amigaos/opforge/opforge_cli.asm`, native opasm selector tables or generated include files, `crates/opforge-vm/src/execution_model/selector_bridge.rs`, `crates/opforge-vm/src/runtime_tests.rs`, `crates/opforge-asm/src/tests.rs`.
  - Full quality gates: `cargo test -p vm vm_runtime_mos6502_selector_ -- --nocapture`; `cargo test -p vm native6502_ -- --nocapture`; `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`; native selector-status FS-UAE smoke is deferred to Item 10 because Item 6 wires the Rust/native6502 harness selector ABI without changing native Amiga assembly; `cargo fmt --all --check`; `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` must return `PASS` for Item 6 before commit.
  - Commit outcome: one commit that maps parsed operand expression values into 6502 candidate classes for the initial subset: implied, immediate, absolute, zero-page where safe, and absolute jump.
  - Definition of done: the selector can choose candidates for `LDA #imm`, `STA abs`, `JMP abs`, and `NOP` without CLI-side 6502 semantic shortcuts.

- [x] Item 7: Add package-backed 6502 encode-instruction execution
  - Source requirement or finding IDs: SR-OPASM-ENCODER, SR-RUST-PARITY
  - Expected files: `crates/opforge-vm/src/native6502.rs`, `crates/opforge-vm/src/native6502_abi.rs`, `crates/opforge-vm/src/runtime_tests.rs`, `crates/opforge-asm/src/tests.rs`.
  - Full quality gates: `cargo test -p vm native6502_ -- --nocapture`; focused tests for `encode_instruction_from_exprs("m6502", None, ...)`; `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`; native CLI FS-UAE encode smoke is deferred to Item 10 because Item 7 wires the Rust/native6502 harness selected-encode ABI and session-image buffer without changing native Amiga assembly; `cargo fmt --all --check`; `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` must return `PASS` for Item 7 before commit.
  - Commit outcome: one commit that encodes the selected 6502 candidates into bytes and writes those bytes into the native6502 harness session image.
  - Definition of done: a native6502 harness run can turn the smoke program instructions into the same bytes as Rust for the supported subset, even if native Amiga CLI output file writing is still stubbed.

- [x] Item 8: Add the minimal native two-pass engine loop
  - Source requirement or finding IDs: SR-ENGINE-PASSES, SR-SYMBOLS
  - Expected files: `examples/motorola68000/amigaos/opforge/opforge_cli.asm`, `crates/opforge-asm/src/tests.rs`, native CLI reference outputs if diagnostics change.
  - Full quality gates: focused pass-surface tests for forward label layout and duplicate-label handling; `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`; `cargo test -p asm examples_match_reference_outputs -- --nocapture`; FS-UAE native CLI pass smoke; `cargo fmt --all --check`; `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` must return `PASS` for Item 8 before commit.
  - Commit outcome: one commit that runs pass 1 to collect origin, current PC, and labels, then pass 2 to repeat deterministic layout while output byte writing remains an Item 9 concern.
  - Definition of done: the native engine records local labels such as `done` at deterministic pass-1 PCs, advances pass-2 layout over the supported smoke subset, and fails deterministically for duplicate labels. Unresolved operand-label negative coverage remains deferred to Item 10 with the operand/output FS-UAE gate.

- [x] Item 9: Add flat binary output and CLI success path
  - Source requirement or finding IDs: SR-OUTPUT-BIN, SR-CLI-CONTRACT
  - Expected files: `examples/motorola68000/amigaos/opforge/opforge_cli.asm`, `crates/opforge-asm/src/tests.rs`, `examples/reference/motorola68000/amigaos/opforge/opforge_cli.*` if the assembled native CLI binary changes.
  - Full quality gates: focused native CLI Rust harness test for `--bin` or the closest Rust CLI-compatible output flag; `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`; `cargo test -p asm examples_match_reference_outputs -- --nocapture`; FS-UAE native CLI binary-output smoke comparing bytes to Rust; `cargo fmt --all --check`; `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` must return `PASS` for Item 9 before commit.
  - Commit outcome: one commit that replaces the emitter-not-implemented path for the 6502 smoke subset with a real success path and output file.
  - Definition of done: running the native CLI against the tiny 6502 source creates a flat output file whose bytes match the Rust CLI reference for the same package, CPU, and input.

- [x] Item 10: Add FS-UAE end-to-end gate and negative-path coverage
  - Source requirement or finding IDs: SR-FS-UAE-GATE, SR-DETERMINISTIC-DIAGNOSTICS
  - Expected files: `crates/opforge-asm/src/fs_uae_smoke.rs`, `crates/opforge-asm/src/tests.rs`, FS-UAE fixture sources, native CLI reference outputs if changed.
  - Full quality gates: FS-UAE native CLI small 6502 assembly gate including selector-status evidence; negative FS-UAE cases for unknown mnemonic, unsupported addressing mode, unresolved label, bad `.org`, and unsupported output; `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`; `cargo test -p asm examples_match_reference_outputs -- --nocapture`; `cargo fmt --all --check`; `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` must return `PASS` for Item 10 before commit.
  - Commit outcome: one commit that makes the 6502 native small-assembly path part of the repeatable FS-UAE quality gates.
  - Definition of done: FS-UAE proves native AmigaOS execution can assemble the smoke source and produce matching bytes, and invalid inputs produce deterministic CLI diagnostics and non-zero status.

## Milestones

- [x] Milestone 1: Contract and state are ready for native assembly without behavior shortcuts.
- [x] Milestone 2: Native VM bridges can parse statement rows, evaluate generic expressions, select 6502 candidates, and encode bytes.
- [x] Milestone 3: Native CLI has a minimal two-pass engine and flat binary output success path.
- [x] Milestone 4: FS-UAE validates the end-to-end path and selected negative cases.

## Deferred Work

- Full Rust CLI output matrix parity beyond flat binary.
- Amiga Hunk output for non-Amiga target CPUs.
- Relocation, object linking, and rich listing/map output.
- Macro expansion and conditional assembly beyond the current native subset.
- Broad CPU-family support after the 6502 vertical slice works.
- LSP, fixits, and editor-service VM integration.
- Performance optimization after correctness is boring.

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- no CPU-specific encoding or operand semantics may be implemented in opcore or
  hidden inside CLI glue
- no expression math parsing or evaluation may be implemented inside PRVM/opasm
  statement parsing or CLI glue
