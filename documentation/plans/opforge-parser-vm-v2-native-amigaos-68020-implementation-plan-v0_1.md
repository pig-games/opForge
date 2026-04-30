# opForge Parser VM v2 Native AmigaOS 68020-Baseline Implementation Plan v0.1

## Metadata

- Source: WI-7 in `documentation/plans/opforge-parser-vm-v2-opasm-statement-implementation-plan-v0_1.md`, completed WI-6 commit `e3699c7c`, `documentation/opForge-extended-parser-vm-instruction-set-spec-v0_1.md`, `documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md`, `documentation/opForge-m68000-tokenizer-vm-single-line-buffer-abi-spec-v0_1.md`, and the current Rust PRVM v2 implementation in `crates/opforge-vm/src/execution_model/parser_vm_v2.rs`.
- Mode: `implementation`
- Owner: GitHub Copilot

## Objective

Plan the first native AmigaOS 68020-baseline implementation path for the PRVM v2
opasm statement parser, using the completed Rust PRVM v2 implementation as the
source of truth.

The native entry symbol is reserved as `prvm_run_68000`, matching the existing
native tokenizer naming convention while targeting `.cpu 68020` for the first
implementation slices.

The native parser starts after tokenizer output and after opcore has delegated a
line to `ProcessingRequestKind::Processor { processor: "asm", kind:
"statement" }`. The completed baseline does not replace opcore line routing,
does not port the opcore expression parser, and does not become a whole-file
assembler pass. Post-baseline extension items may add a narrow native line
router and whole-file line iterator around the same single-line tokenizer/PRVM
contracts, but those wrappers must not move expression parsing, CPU-family
semantics, macro expansion, symbol resolution, or instruction encoding into the
native parser.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- This plan must not become active until `plan-quality-reviewer` or
  `plan-quality-orchestrator` returns `PASS`.
- One active work item at a time.
- Each work item or phase must end in exactly one new commit before the next
  item begins.
- Full quality gates are mandatory before each commit.
- `plan-compliance-reviewer` must pass before each plan-driven commit.
- The first native target is AmigaOS `.cpu 68020`, but the ABI entry symbol is
  reserved as `prvm_run_68000` for continuity with the tokenizer-native symbol
  family.
- Rust PRVM v2 remains the behavioral authority. Native results must be compared
  against `parse_statement_line_with_model` and the `parser_vm_v2_parity` corpus
  before any native behavior is treated as correct.
- The native PRVM must execute package-provided parser VM bytecode. It must not
  hard-code CPU-family statement semantics in interpreter control flow.
- The first native ABI is single-line and delegated-statement only. It must not
  accept newline-containing source as a whole-file parse request.
- Tokenization remains outside this plan except for consuming the already
  documented token-buffer/lexeme-buffer shape produced by the tokenizer-native
  path or an equivalent host-side fixture.
- Expression parsing remains Rust/opcore-owned for production behavior. Native
  PRVM requests expression parsing only through a host-mediated sub-call protocol
  over explicit operand token ranges. A smoke-only native caller harness may
  synthesize a literal expression slot from a fixed one-token fixture to prove
  the pass-back ABI in FS-UAE, but that shim is not production expression
  parsing and must not move into interpreter control flow.
- No production native expression parser, native instruction encoder, macro
  expander, module graph, or full assembler pass may be introduced by this plan.
  The only exception is the smoke-only one-token literal pass-back shim described
  above.
- FS-UAE or other emulator execution may be opt-in evidence, but it must not
  become a default required dependency for local or CI quality gates.
- Fixture/reference regeneration is allowed only when a work item intentionally
  changes an assembled native artifact or report format, and the change must be
  recorded in that work item's validation evidence.

## Planning Decisions Captured Up Front

- The first implementation work item defines a native PRVM ABI/spec before
  landing assembly. The ABI mirrors the tokenizer single-line buffer style:
  caller-owned source/token/lexeme input, caller-owned output/status buffers,
  deterministic return registers, and big-endian memory records.
- `prvm_run_68000` consumes token records rather than raw source bytes as its
  primary input. The source and lexeme buffers remain available only so spans,
  token text, diagnostics, and report rendering can match Rust behavior.
- Native PRVM emits a compact parser-result/event buffer rather than trying to
  materialize Rust `LineAst` values in Amiga memory. Host-side tests decode that
  buffer and compare the decoded AST or diagnostic against Rust PRVM v2.
- Expression sub-calls use an explicit pause/resume protocol: native PRVM returns
  a deterministic "expression requested" status with the operand token range and
  continuation state; the host calls the existing Rust opcore expression parser,
  writes the result slot back to the native buffers, and resumes `prvm_run_68000`.
- The first native interpreter slice should prove one narrow happy path and one
  expression sub-call path before broadening to every PRVM v2 opcode.
- The AmigaOS CLI/file I/O harness is deliberately deferred until the native ABI
  and host-side parity harness are stable.
- The first post-baseline line-routing slice should connect existing native
  tokenizer and PRVM statement components for exactly one delegated line before
  any whole-file input loop is introduced.
- The first whole-file iteration slice should split input into deterministic
  newline-free logical lines and call the same line router one line at a time;
  it is not a macro/module/full-assembler pass.

## Work Items

- [x] Work item 1: define the native PRVM single-line ABI and host bridge contract
  - Source requirement or finding IDs: WI-7 requirement to reserve `prvm_run_68000`; tokenizer single-line ABI model; Rust PRVM v2 entry-boundary and expression sub-call contracts.
  - Validation: see the focused spec check and full quality gates listed below.
  - Definition of done: see detailed criteria below for this work item.
  - Expected files:
    - `documentation/opForge-m68000-parser-vm-single-line-buffer-abi-spec-v0_1.md` (new)
    - optionally, `documentation/opForge-extended-parser-vm-instruction-set-spec-v0_2.md` if the quality gate requires a spec reissue rather than a native ABI companion spec
  - Implementation notes:
    - added `documentation/opForge-m68000-parser-vm-single-line-buffer-abi-spec-v0_1.md` as the native PRVM ABI authority for later host-side decode tests and native assembly slices
    - the spec reserves `prvm_run_68000` as the only native PRVM entry symbol while keeping `.cpu 68020` as the first native baseline
    - the ABI uses a caller-owned request frame containing source, token, lexeme, parser-program, result, diagnostic, resume, expression-request, and expression-result buffer pointers and capacities
    - result/event, diagnostic, expression-request, expression-result, and resume-state records are defined as fixed big-endian records
    - expression parsing remains Rust/opcore-owned through an explicit expression-request status and half-open token-range pause/resume protocol
    - no native parser assembly, fixture, Rust parser behavior change, or spec reissue was landed in this work item
  - Validation details:
    - `python3 scripts/workflow/check_spec_artifact.py documentation/opForge-m68000-parser-vm-single-line-buffer-abi-spec-v0_1.md`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
  - Validation evidence:
    - `python3 scripts/workflow/check_spec_artifact.py documentation/opForge-m68000-parser-vm-single-line-buffer-abi-spec-v0_1.md` passed
    - first `spec-quality-reviewer` run returned `FAIL` because the ABI did not define `D1`/`D2`/`D3` semantics for every status and left expression-result error payload ownership ambiguous
    - after adding status-specific return-register semantics and making `host_expr_handle` the sole authoritative expression payload for both ready expressions and ready expression errors, the `spec-quality-reviewer` rerun returned `PASS`
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-parser-vm-v2-native-amigaos-68020-implementation-plan-v0_1.md` passed
    - `cargo fmt --all` passed
    - `cargo clippy --all-targets --all-features -- -D warnings` passed
    - `cargo audit` with network fetch failed before scanning because the RustSec advisory database fetch hit an IO error; `cargo audit --no-fetch` passed using the local advisory cache with the two already-known allowed warnings (`registry` unmaintained, `rand` advisory through `proptest`)
    - `cargo test --workspace` is accepted for this WI with `864 passed; 1 failed`, where the only failure is the previously user-waived baseline `asm::tests::examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returned `PASS`; the slice is limited to the new ABI spec and plan bookkeeping, with no native parser assembly, fixtures, Rust parser behavior changes, or spec reissue
  - Commit outcome:
    - the native PRVM ABI authority exists and defines `prvm_run_68000`, register usage, input token/source/lexeme buffers, output result records, diagnostic/status returns, pause/resume expression sub-calls, and single-line rejection rules
  - Definition of done:
    - the spec states that the active worktree `AGENTS.md` workflow and execution rules remain binding for any derived implementation
    - `prvm_run_68000` is the only reserved native PRVM entry symbol
    - `.cpu 68020` is the first native baseline while the symbol name remains `prvm_run_68000`
    - the input ABI references caller-owned token records plus source/lexeme buffers rather than raw whole-file parsing
    - the output ABI defines deterministic status codes, AST/result records, diagnostic records, cursor/span reporting, and resume state storage
    - the host-mediated expression sub-call protocol names the exact token-range fields the host must parse with Rust opcore and the result slot the host writes back before resume
    - newline-containing source and non-delegated entry requests are explicit failure cases
    - no native parser assembly or fixture is landed in this work item

- [x] Work item 2: add host-side PRVM native ABI fixtures and decode/report tests
  - Source requirement or finding IDs: Work item 1 ABI; WI-6 parity corpus; tokenizer-native host-side ABI decode pattern.
  - Validation: see the focused ABI decode tests and full quality gates listed below.
  - Definition of done: see detailed criteria below for this work item.
  - Expected files:
    - `crates/opforge-asm/src/tests.rs` or a focused native-parser test module if one exists by implementation time
    - `crates/opforge-vm/tests/parser_vm_v2_parity.rs` only if the parity corpus needs a shared fixture helper
    - native ABI fixture data under the existing examples/reference path only if required for deterministic decode tests
  - Implementation notes:
    - added `crates/opforge-vm/tests/parser_vm_native_abi.rs` as the focused host-side native PRVM ABI decode test surface
    - the tests use synthetic fixed big-endian ABI records from the Work item 1 spec rather than native interpreter output, keeping this slice ahead of assembly as planned
    - success records decode to a `LineAst::Statement` shape and compare against Rust PRVM v2 output from `parse_statement_line_with_model`
    - diagnostic records decode stable diagnostic codes, messages, token indexes, and spans
    - expression-request and expression-result slot tests cover bounded token ranges, resume-slot identity, host-owned expression handles, ready expression-error slots, and reserved-field validation
    - newline-containing input and non-delegated entry requests are locked as deterministic native status returns
    - no native interpreter assembly, reference artifact, or Rust parser behavior change was landed in this work item
    - full-gate clippy surfaced pre-existing toolchain lints outside the ABI test surface; this slice includes only the mechanical behavior-preserving lint fixes needed to unblock `cargo clippy --all-targets --all-features -- -D warnings`
  - Validation details:
    - focused host-side PRVM ABI decode tests for success, diagnostic, entry-boundary failure, expression-request pause, expression-result resume, and newline rejection
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
  - Validation evidence:
    - `cargo test -p vm native_prvm_abi -- --nocapture` passed (`5` focused host-side ABI decode tests)
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture` passed (`2` private executor tests plus `7` integration tests)
    - `cargo fmt --all` passed
    - initial full clippy run failed on three pre-existing current-toolchain lints: redundant `.into_iter()` calls in `crates/opforge-package/src/package/codec/scoped_schema.rs`, collapsible guarded matches in `crates/opforge-families/src/m45gs02/handler.rs`, and a collapsible guarded match in `crates/opforge-vm/src/builder.rs`; each was fixed mechanically without changing runtime behavior
    - `cargo clippy --all-targets --all-features -- -D warnings` passed after those gate-unblock fixes
    - `cargo audit --no-fetch` passed using the local advisory cache with the two already-known allowed warnings (`registry` unmaintained, `rand` advisory through `proptest`)
    - `cargo test --workspace` is accepted for this WI with `864 passed; 1 failed`, where the only failure is the previously user-waived baseline `asm::tests::examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returned `PASS`; the slice is limited to host-side native PRVM ABI decode tests and plan bookkeeping, with only mechanical clippy gate-unblock edits outside the ABI test surface and no native assembly, reference artifact, or Rust parser behavior change
  - Commit outcome:
    - host-side tests can decode native PRVM buffers and compare their normalized AST/diagnostic shape against Rust PRVM v2 expectations before assembly code lands
  - Definition of done:
    - tests decode fixed success records into the same normalized statement shape used by the Rust v2 parity corpus
    - tests decode diagnostic records into stable parser diagnostic messages and spans
    - tests prove the expression-request pause record contains a bounded operand token range and a resume slot
    - tests prove newline-containing input and non-delegated entry requests map to deterministic native status values
    - no native interpreter assembly is landed in this work item

- [x] Work item 3: land the first `prvm_run_68000` native interpreter slice over caller-owned buffers
  - Source requirement or finding IDs: Work items 1 and 2; Rust PRVM v2 control-flow/checkpoint behavior; WI-6 parity corpus.
  - Validation: see the focused native assembly/parity tests and full quality gates listed below.
  - Definition of done: see detailed criteria below for this work item.
  - Expected files:
    - `examples/motorola68000/amigaos/prvm/prvm_interpreter.asm` (new)
    - `examples/reference/motorola68000/amigaos/prvm_interpreter.hunk` and `.lst` if the reference workflow requires committed artifacts
    - focused host-side tests in `crates/opforge-asm/src/tests.rs` or a dedicated native PRVM test surface
  - Validation details:
    - focused native PRVM assembly/reference test for `prvm_run_68000`
    - focused host-side decode test comparing a no-expression statement result against Rust PRVM v2
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
  - Implementation notes:
    - added `examples/motorola68000/amigaos/prvm/prvm_interpreter.asm` as the first `.cpu 68020` native PRVM module exporting `prvm_run_68000`
    - the interpreter validates the v0.1 caller-owned request frame, rejects newline input before execution, preserves callee-owned registers, and returns deterministic ABI statuses for invalid arguments, entry-boundary mismatches, malformed tokens, invalid programs, unsupported opcodes, output overflow, and step-budget exhaustion
    - the implemented opcode subset is intentionally narrow: `BeginStatement`, `ParseOptionalLeadingLabel` as a first-slice no-op, `LoadIdentifier`, `SetMnemonic`, `Advance`, `FinishLine`, and `End`
    - result emission writes only caller-owned 32-byte result records for `BEGIN_STATEMENT`, `MNEMONIC_TEXT`, and `FINISH_LINE`; expression sub-calls and operand records remain deferred to Work item 4
    - generated and committed the initial standalone reference Hunk/listing artifacts required by the example reference workflow at `examples/reference/motorola68000/amigaos/prvm_interpreter.hunk` and `.lst`; these are superseded by Work item 3a, where the interpreter becomes an importable module and the executable smoke wrapper owns the reference artifacts
    - extended the host ABI decode tests with an indented no-expression `NOP` statement so Rust PRVM v2 treats it as a mnemonic rather than a column-1 bare label
    - no AmigaOS CLI/file-I/O harness and no native expression parser were added in this work item
  - Validation evidence:
    - `cargo test -p asm motorola68020_prvm_interpreter_example_assembles_first_native_slice -- --nocapture` passed
    - `cargo test -p vm native_prvm_abi -- --nocapture` passed (`6` focused host-side ABI decode tests)
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture` passed (`2` private executor tests plus `7` integration tests)
    - `cargo fmt --all` and `cargo fmt --all -- --check` passed
    - `cargo clippy --all-targets --all-features -- -D warnings` passed
    - `cargo audit --no-fetch` passed using the local advisory cache with the two already-known allowed warnings (`registry` unmaintained, `rand` advisory through `proptest`)
    - `cargo test --workspace` is accepted for this WI with `865 passed; 1 failed`, where the only failure is the previously user-waived baseline `asm::tests::examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returned `PASS`; the slice is limited to the first caller-owned-buffer native interpreter path, focused assembly/reference and host decode tests, and plan bookkeeping, with no expression pause/resume implementation and no AmigaOS CLI/file-I/O harness
  - Commit outcome:
    - `prvm_run_68000` exists as a `.cpu 68020` native PRVM interpreter entrypoint and can produce one decoded statement result through the ABI for a newline-free delegated opasm statement
  - Definition of done:
    - the assembly exports `prvm_run_68000` and preserves the ABI's callee-saved register contract
    - the first opcode subset is only as broad as needed for one delegated statement path without expression sub-calls
    - the interpreter consumes package-provided PRVM bytecode and token buffers rather than embedding 68000 statement semantics
    - deterministic failure statuses exist for invalid arguments, invalid program bytes, unsupported opcodes, output-buffer overflow, newline input, and entry-boundary mismatch
    - host-side tests decode the native output and compare it with Rust PRVM v2 for the same line
    - no AmigaOS CLI/file I/O harness is added in this work item

- [x] Work item 3a: add a minimal FS-UAE smoke executable for the first native PRVM slice
  - Source requirement or finding IDs: user-requested native/UAE smoke coverage before expression work; existing `external_fs_uae_hunk_smoke` infrastructure; Work item 3 `prvm_run_68000` callable module.
  - Validation: see focused smoke/reference tests below.
  - Definition of done: see detailed criteria below for this work item.
  - Expected files:
    - `examples/motorola68000/amigaos/prvm/prvm_interpreter.asm`
    - `examples/motorola68000/amigaos/prvm/prvm_smoke.asm`
    - `examples/reference/motorola68000/amigaos/prvm_smoke.hunk` and `.lst`
    - `crates/opforge-asm/src/fs_uae_smoke.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `documentation/amigaos-hunk-post-v0_3-pickups.md`
  - Validation details:
    - focused PRVM smoke assembly/reference test for the `prvm_run_68000` call surface
    - focused m68k example reference-workflow test
    - existing opt-in FS-UAE smoke test entry point, with a clean skip when `OPFORGE_FS_UAE_SMOKE` is not set
    - focused host-side native PRVM ABI decode tests
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture`
  - Implementation notes:
    - converted `prvm_interpreter.asm` into an importable module by removing the root-only `.output` directive and keeping the focused test as a source-surface check for the exported native entrypoint
    - added `prvm_smoke.asm` as a minimal `.cpu 68020` AmigaOS process-entry smoke executable that opens `dos.library`, builds a caller-owned PRVM request frame for the indented `" NOP"` no-expression statement, calls `prvm_run_68000`, validates the three emitted result records, and prints `OPFORGE-PRVM smoke OK` on success
    - registered `prvm_smoke` with the existing opt-in FS-UAE Hunk smoke runner and added an output-marker assertion for `OPFORGE-PRVM smoke OK`
    - generated `prvm_smoke` Hunk/listing references and removed the superseded standalone `prvm_interpreter` references
    - fixed the native result-record helper so successful nonzero result offsets clear `D0` before returning to emit callers; the real FS-UAE run exposed the bug as a native `OPFORGE-PRVM smoke FAIL status $00000020` before the fix
    - kept this checkpoint narrower than the later Work item 6 demo/report harness: no file I/O, no expression pause/resume, no native expression parser, and no broad report format
    - recorded a future Hunk post-v0.3 pickup note for symbolic code/data relocation limitations exposed while making the smoke executable Hunk-compatible
  - Validation evidence:
    - `cargo test -p asm motorola68020_prvm_smoke_example_assembles_with_native_call_surface -- --nocapture` passed
    - `cargo test -p asm motorola68020_prvm_interpreter_example_assembles_first_native_slice -- --nocapture` passed
    - `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture` passed
    - `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test -p asm external_fs_uae_hunk_smoke -- --nocapture` passed; the run completed `helloworld`, `writefile`, `tkpkg_debug_cli`, and `prvm_smoke`, and the PRVM smoke assertion observed `OPFORGE-PRVM smoke OK` from native AmigaOS execution
    - `cargo test -p vm native_prvm_abi -- --nocapture` passed (`6` focused host-side ABI decode tests)
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture` passed (`2` private executor tests plus `7` integration tests)
    - `cargo fmt --all -- --check` passed after applying `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings` passed
    - `cargo audit --no-fetch` completed with the existing allowed warnings for `registry` (`RUSTSEC-2025-0026`) and `rand` (`RUSTSEC-2026-0097`)
    - `scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-parser-vm-v2-native-amigaos-68020-implementation-plan-v0_1.md` passed
    - `cargo test --workspace` remains accepted for this WI with `866 passed; 1 failed`, where the only failure is the previously user-waived baseline `asm::tests::examples_match_reference_outputs` broad reference mismatch
  - Plan-compliance review evidence:
    - first `plan-compliance-reviewer` pass returned `FAIL` only because this plan section still contained pending bookkeeping; the reviewer found no separate scope, behavior, or validation blocker for Work item 3a
    - `plan-compliance-reviewer` rerun returned `PASS`; Work item 3a remains limited to the importable `prvm_run_68000` smoke executable path, replacement `prvm_smoke` reference artifacts, focused FS-UAE/native validation, and bookkeeping only, with the previously accepted `asm::tests::examples_match_reference_outputs` workspace-test mismatch still the sole recorded exception for this work item
  - Commit outcome:
    - this boundary commit records the required Work item 3a implementation after the compliance `PASS`
  - Definition of done:
    - the smoke executable is a root Hunk example that calls the importable `prvm_run_68000` module through caller-owned buffers
    - the smoke validates the first no-expression native PRVM result path before printing the success marker
    - the smoke is wired into the existing opt-in FS-UAE smoke runner without making emulator execution a default dependency
    - reference artifacts cover the root smoke executable, while the interpreter remains importable and is not treated as a standalone example
    - the slice does not broaden PRVM semantics beyond Work item 3

- [x] Work item 4: implement host-mediated opcore expression sub-calls for native PRVM
  - Source requirement or finding IDs: Work item 1 pause/resume ABI; Rust PRVM v2 `ParseOperandExprRange`; WI-6 typed expression sub-call assertions.
  - Validation: see the focused expression pause/resume tests and full quality gates listed below.
  - Definition of done: see detailed criteria below for this work item.
  - Current sub-slice 4a progress:
    - implemented the native `ParseOperandExprRange` dynamic operand pause path for explicit expression-request records
    - added resume-state validation for caller-filled expression-result slots and native `OPERAND_EXPR_SLOT` result emission
    - expanded the FS-UAE PRVM smoke from ` NOP` to ` LDA #42`; the smoke now validates `PRVM_STATUS_EXPR_REQUEST`, fills expression slot `0`, resumes with `call_mode = 1`, and validates the final begin/mnemonic/operand-slot/finish result sequence
    - fixed the native operand range scan so token-pointer helper register clobbering cannot widen the requested token range
    - fixed expression request boundary fields to use the request-frame line number plus token-record columns
  - Current sub-slice 4a validation evidence:
    - `cargo test -p asm motorola68020_prvm_interpreter_example_assembles_first_native_slice -- --nocapture` passed
    - `cargo test -p asm motorola68020_prvm_smoke_example_assembles_with_native_call_surface -- --nocapture` passed
    - `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test -p asm external_fs_uae_hunk_smoke -- --nocapture` passed for `helloworld`, `writefile`, `tkpkg_debug_cli`, and expression-bearing `prvm_smoke`
    - `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture` passed after regenerating `prvm_smoke` references
    - `cargo test -p vm native_prvm_abi -- --nocapture` passed, 6 tests
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture` passed, 2 unit tests and 7 integration tests
    - `cargo fmt --all -- --check` passed after applying `cargo fmt --all` for one long focused-test assertion
    - `cargo clippy --all-targets --all-features -- -D warnings` passed
    - `cargo audit --no-fetch` completed with the existing allowed `registry` and `rand` warnings
    - `/Users/erik/Code/Retro/opForge/.venv/bin/python scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-parser-vm-v2-native-amigaos-68020-implementation-plan-v0_1.md` passed
    - `cargo test --workspace` retained the previously accepted broad baseline exception: `866 passed; 1 failed`, with the remaining failure still in `asm::tests::examples_match_reference_outputs`
  - Current sub-slice 4a remaining work before closing Work item 4:
    - drive the host bridge through multi-operand native pause/resume coverage with host-owned expression slots
    - preserve more `Expr::Error` parity cases through the host bridge, beyond the first empty-range error slot
  - Current sub-slice 4b progress:
    - added `vm::native_prvm` as a non-native Rust/test-tooling bridge helper for native PRVM expression request buffers; this helper is not part of the FS-UAE Amiga runtime and does not imply Rust is present inside the native 68020 process
    - the bridge decodes a 32-byte native expression-request record, validates the token range against Rust parser tokens, invokes the existing Rust/opcore operand expression parser, stores the parsed `Expr` by host handle and native slot index, and writes a 32-byte native expression-result slot for resume
    - expression-result slot state now distinguishes ready expressions from ready `Expr::Error` results so native resume can preserve error slots without parsing expressions locally
    - focused ABI coverage proves `LDA #42` is parsed through the bridge and decoded back into the same Rust PRVM v2 statement shape through an `OPERAND_EXPR_SLOT` record
    - focused ABI coverage also proves an empty native expression range is preserved as a ready `Expr::Error` slot
  - Current sub-slice 4b validation evidence:
    - `cargo test -p vm native_prvm_abi -- --nocapture` passed, including the new host-bridge expression slot and `Expr::Error` slot tests (`8` focused tests total)
    - `cargo fmt --all -- --check` passed after applying `cargo fmt --all`
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture` passed (`2` unit tests and `7` integration tests)
    - `cargo test -p vm` passed (`292` unit tests, `8` native ABI integration tests, and `7` parser parity integration tests)
    - `cargo clippy -p vm --all-targets --all-features -- -D warnings` passed
    - `cargo clippy --all-targets --all-features -- -D warnings` passed
    - `cargo audit --no-fetch` completed with the existing allowed `registry` and `rand` warnings
    - `/Users/erik/Code/Retro/opForge/.venv/bin/python scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-parser-vm-v2-native-amigaos-68020-implementation-plan-v0_1.md` passed
    - `cargo test --workspace` retained the previously accepted broad baseline exception: `866 passed; 1 failed`, with the remaining failure still in `asm::tests::examples_match_reference_outputs`
  - Current sub-slice 4b remaining work before closing Work item 4:
    - add multi-operand native pause/resume coverage driven by host-owned expression slots
    - broaden `Expr::Error` parity through the host bridge beyond the empty-range slot covered in this slice
  - Current sub-slice 4c progress:
    - replaced the `prvm_smoke` hardcoded expression-result fill with a smoke-only native 68020 caller-side pass-back shim for the first literal-only fixture case
    - the native smoke now reads the expression request record, validates that the requested token range contains exactly one token, decodes the fixed immediate decimal fixture lexeme shaped like `#42`, writes a caller-owned native expression slot table record, writes the ABI expression-result slot, and resumes `prvm_run_68000`
    - the smoke validates that the native expression slot table contains a ready immediate-decimal expression with value `42`, source span `1:6..9`, and the requested native slot index before accepting the resumed parser result
    - this proves the native Amiga caller can pass a simple literal expression value back to the native PRVM without Rust/opcore participating in the native runtime; it does not replace the production Rust/opcore host-mediated expression service required by the plan-proper path
  - Current sub-slice 4c validation evidence:
    - `cargo test -p asm motorola68020_prvm_smoke_example_assembles_with_native_call_surface -- --nocapture` passed after adding source-surface assertions for the native expression service and native expression slot validation
    - `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test -p asm external_fs_uae_hunk_smoke -- --nocapture` passed for `helloworld`, `writefile`, `tkpkg_debug_cli`, and native literal-passback `prvm_smoke`
    - regenerated `prvm_smoke` Hunk/listing references from the native literal-passback output
    - `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture` passed
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture` passed (`2` unit tests and `7` integration tests)
    - `cargo fmt --all -- --check` passed
    - `/Users/erik/Code/Retro/opForge/.venv/bin/python scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-parser-vm-v2-native-amigaos-68020-implementation-plan-v0_1.md` passed before this plan-contract clarification
    - `cargo clippy --all-targets --all-features -- -D warnings` passed
    - `cargo audit --no-fetch` completed with the existing allowed `registry` and `rand` warnings
    - `cargo test --workspace` retained the previously accepted broad baseline exception: `866 passed; 1 failed`, with the remaining failure still in `asm::tests::examples_match_reference_outputs`; focused investigation confirmed the refreshed `prvm_smoke.hunk` matches and the broad sweep still includes `tkpkg_debug_cli` reference drift outside this slice
  - Current sub-slice 4c remaining work before closing Work item 4:
    - return to the plan-proper parity path by extending expression service coverage beyond one immediate decimal token
    - add multi-operand native pause/resume coverage driven by host-owned expression slots
    - broaden `Expr::Error` parity through the covered expression service path
  - Current sub-slice 4d progress:
    - returned to the plan-proper host-mediated expression path with focused Rust/test-tooling ABI coverage only; no native production expression parser or FS-UAE smoke shim expansion was added
    - added m68k `MOVE.B D0,D1` coverage that services two native expression-request records through `NativePrvmHostExpressionBridge`
    - verified the bridge stores two host-owned expression handles/native slots and writes two ready expression-result slots for resume
    - decoded a native result containing two `PRVM_RESULT_OPERAND_EXPR_SLOT` records back into the same Rust PRVM v2 statement shape for the m68020 authority parser
  - Current sub-slice 4d validation evidence:
    - `cargo test -p vm native_prvm_abi -- --nocapture` passed, including the new multi-operand host-bridge slot test (`9` focused tests total)
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture` passed (`2` unit tests and `7` integration tests)
    - `cargo fmt --all -- --check` passed
    - `/Users/erik/Code/Retro/opForge/.venv/bin/python scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-parser-vm-v2-native-amigaos-68020-implementation-plan-v0_1.md` passed
    - `cargo test -p vm` passed (`292` unit tests, `9` native ABI integration tests, and `7` parser parity integration tests)
    - `cargo clippy -p vm --all-targets --all-features -- -D warnings` passed
    - `cargo clippy --all-targets --all-features -- -D warnings` passed
    - `cargo audit --no-fetch` completed with the existing allowed `registry` and `rand` warnings
    - `cargo test --workspace` retained the previously accepted broad baseline exception: `866 passed; 1 failed`, with the remaining failure still in `asm::tests::examples_match_reference_outputs`
  - Current sub-slice 4d remaining work before closing Work item 4:
    - broaden `Expr::Error` parity through the covered host-mediated expression service path beyond the empty-range error slot
  - Current sub-slice 4e progress:
    - broadened host-mediated `Expr::Error` parity with focused Rust/test-tooling ABI coverage only; no native production expression parser or FS-UAE smoke shim expansion was added
    - added malformed non-empty operand coverage for `LDA 1 +` that drives token range `1..3` through `NativePrvmHostExpressionBridge`
    - verified the bridge writes a ready-error expression-result slot for the host-owned native slot and preserves the non-empty expression parser error through native result decoding
    - compared the decoded native result back to Rust PRVM v2 so the normalized error message and end-of-line span remain host-authority driven
  - Current sub-slice 4e validation evidence:
    - `cargo test -p vm native_prvm_abi -- --nocapture` passed, including the new non-empty malformed expression error slot test (`10` focused tests total)
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture` passed (`2` unit tests and `7` integration tests)
    - `/Users/erik/Code/Retro/opForge/.venv/bin/python scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-parser-vm-v2-native-amigaos-68020-implementation-plan-v0_1.md` passed before the final evidence update
    - `cargo fmt --all` applied one assertion formatting change, then `cargo fmt --all -- --check` passed
    - `cargo test -p vm` passed (`292` unit tests, `10` native ABI integration tests, and `7` parser parity integration tests)
    - `cargo clippy -p vm --all-targets --all-features -- -D warnings` passed
    - `cargo clippy --all-targets --all-features -- -D warnings` passed
    - `cargo audit --no-fetch` completed with the existing allowed `registry` and `rand` warnings
    - `cargo test --workspace` retained the previously accepted broad baseline exception: `866 passed; 1 failed`, with the remaining failure still in `asm::tests::examples_match_reference_outputs`
    - Plan Compliance Reviewer passed for this 4e boundary slice and allowed committing only `crates/opforge-vm/tests/parser_vm_native_abi.rs` plus this plan ledger update
  - Current sub-slice 4e remaining work before closing Work item 4:
    - complete; committed as `46d8e2f7 Add native PRVM expression error bridge parity`
  - Expected files:
    - `examples/motorola68000/amigaos/prvm/prvm_interpreter.asm`
    - host-side bridge/decode tests in the existing native PRVM test surface
    - optional small helper in `crates/opforge-vm` only if required to reuse Rust opcore expression parsing from tests without duplicating logic
  - Validation details:
    - focused native PRVM expression-request/resume tests for one operand and multiple operand ranges
    - focused parity test comparing expression-bearing native results against Rust PRVM v2, including `Expr::Error` preservation
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returned `PASS` for the boundary 4a slice limited to native expression-request/resume mechanics, caller-filled expression slots, and `LDA #42` FS-UAE smoke validation; Work item 4 remains open for the Rust/opcore host bridge, multi-operand coverage, and `Expr::Error` parity
    - `plan-compliance-reviewer` returned `PASS` for the boundary 4b slice limited to the host-side `vm::native_prvm` expression bridge helper, focused `LDA #42` host-bridge AST reconstruction, and first empty-range `Expr::Error` slot preservation; Work item 4 remains open for multi-operand pause/resume coverage and broader `Expr::Error` parity
    - `plan-quality-reviewer` returned `PASS` for the plan-contract clarification that permits only a smoke-only one-token literal pass-back shim while keeping production expression parsing Rust/opcore-owned
    - `plan-compliance-reviewer` returned `PASS` for the boundary 4c slice limited to the native smoke caller-side `#42` pass-back shim, focused assembly/reference checks, FS-UAE smoke validation, and refreshed `prvm_smoke` references; Work item 4 remains open for the plan-proper host-mediated parity path, multi-operand coverage, and broader `Expr::Error` parity
    - `plan-compliance-reviewer` returned `PASS` for the boundary 4d slice limited to multi-operand host-bridge ABI coverage for m68020 `MOVE.B D0,D1`, focused validation, and plan evidence; Work item 4 remains open for broader host-mediated `Expr::Error` parity
    - `plan-compliance-reviewer` returned `PASS` for the boundary 4e slice limited to non-empty malformed `Expr::Error` host-bridge parity for `LDA 1 +`, focused validation, and plan evidence; Work item 4 is complete after commit `46d8e2f7`
  - Commit outcome:
    - native PRVM can request Rust/opcore expression parsing over explicit operand token ranges and resume with returned expression slots while preserving Rust PRVM v2 AST and `Expr::Error` behavior
  - Definition of done:
    - native PRVM returns the ABI-defined expression-request status rather than attempting to parse expressions locally
    - the host bridge invokes the existing Rust opcore expression parser for the requested token range
    - returned expression slots are inserted into the decoded native parser result in operand order
    - expression errors are preserved in the same normalized shape as Rust PRVM v2 for the covered cases
    - multiple expression sub-calls in one statement remain deterministic and bounded

- [x] Work item 5: broaden native PRVM opcode and statement parity to the WI-6 authority corpus
  - Source requirement or finding IDs: WI-6 `parser_vm_v2_parity` corpus; Rust PRVM v2 source-of-truth requirement; native-port readiness gate.
  - Validation: see current item-level validation evidence below; focused PRVM guards, native ABI tests, Rust PRVM v2 parity tests, scoped reference refresh, fmt, clippy, audit, diff-check, and workspace baseline check were run.
  - Definition of done: see detailed criteria and current item-level completion progress below for this work item.
  - Current sub-slice 5a progress:
    - started WI-6 parity broadening with native leading-label statement support, the smallest missing statement-shape surface after Work item 4 expression bridging
    - implemented native `ParseOptionalLeadingLabel` handling for tokenizer-ABI identifier/colon tokens and `LABEL_TEXT` result emission in `prvm_run_68000`
    - reset native statement-local label and operand state at `BEGIN_STATEMENT` so label metadata does not leak between parse attempts or resume paths
    - kept expression parsing host-mediated; this slice does not add native expression parsing or broaden the smoke-only literal pass-back shim
    - added focused assembly-source guards so the native PRVM module keeps the label opcode/result surface visible while broader executable native parity tests are deferred
  - Current sub-slice 5a validation evidence:
    - `cargo test -p asm motorola68020_prvm_interpreter_example_assembles_first_native_slice -- --nocapture` passed
    - `cargo test -p asm motorola68020_prvm_smoke_example_assembles_with_native_call_surface -- --nocapture` passed
    - `scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-parser-vm-v2-native-amigaos-68020-implementation-plan-v0_1.md` passed
    - `cargo test -p vm native_prvm_abi -- --nocapture` passed, including 10 native ABI bridge tests
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture` passed, including 2 unit tests and 7 parity integration tests
    - `cargo fmt --all -- --check` passed
    - `opForge_UPDATE_REFERENCE=1 cargo test -p asm tests::examples_match_reference_outputs -- --nocapture` passed and refreshed the intentional `prvm_smoke` hunk/listing drift caused by native interpreter growth; unrelated generated reference refreshes were pruned
    - `cargo test -p asm tests::examples_match_reference_outputs -- --nocapture` remains on the accepted broad reference baseline after pruning unrelated generated references; the original `prvm_smoke` payload mismatch is resolved by the retained PRVM smoke reference refresh
    - `cargo clippy --all-targets --all-features -- -D warnings` passed
    - `cargo audit --no-fetch` completed with the two existing allowed advisories for `registry` and `rand`
    - `git diff --check` passed after trimming trailing whitespace from the regenerated `prvm_smoke.lst` reference listing
    - `cargo test --workspace` retained the previously accepted broad baseline exception: the `asm` crate reported `866 passed; 1 failed`, with the only failure in `tests::examples_match_reference_outputs` after the intentional `prvm_smoke` reference refresh was retained and unrelated generated references were pruned
    - first `plan-compliance-reviewer` pass returned `FAIL` only for missing full `cargo test --workspace` evidence and unfinished ledger bookkeeping; it accepted the 5a slice boundary, changed-file set, native leading-label implementation, guard assertions, and refreshed `prvm_smoke` references
    - second `plan-compliance-reviewer` pass returned `PASS` and allowed committing only `examples/motorola68000/amigaos/prvm/prvm_interpreter.asm`, `crates/opforge-asm/src/tests.rs`, this plan ledger, `examples/reference/motorola68000/amigaos/prvm_smoke.hunk`, and `examples/reference/motorola68000/amigaos/prvm_smoke.lst`
  - Current sub-slice 5a remaining work before closing this boundary slice:
    - complete; ready to commit as the approved Work item 5a boundary slice
  - Current sub-slice 5b progress:
    - made the already-implemented native leading-label opcode observable through the AmigaOS `prvm_smoke` executable path
    - changed the smoke source from expression-only ` LDA #42` to labeled `start: LDA #42`
    - inserted `PRVM_OPCODE_PARSE_OPTIONAL_LABEL` into the smoke parser program and expanded the smoke token stream to label identifier, colon, mnemonic, and operand tokens
    - added smoke validation for the emitted `PRVM_RESULT_LABEL_TEXT` record while preserving the existing smoke-only `#42` host-mediated expression pass-back shim
    - refreshed only the scoped `prvm_smoke` hunk/listing references after pruning unrelated generated reference drift
  - Current sub-slice 5b validation evidence:
    - `cargo test -p asm motorola68020_prvm_smoke_example_assembles_with_native_call_surface -- --nocapture` passed
    - `cargo test -p asm motorola68020_prvm_interpreter_example_assembles_first_native_slice -- --nocapture` passed
    - `cargo test -p vm native_prvm_abi -- --nocapture` passed, including 10 native ABI bridge tests
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture` passed, including 2 unit tests and 7 parity integration tests
    - `cargo fmt --all -- --check` passed
    - `opForge_UPDATE_REFERENCE=1 cargo test -p asm tests::examples_match_reference_outputs -- --nocapture` passed and refreshed the intentional `prvm_smoke` references; unrelated `tkpkg_debug_cli` and `tokvm_interpreter` reference drift was pruned
    - `cargo test -p asm tests::examples_match_reference_outputs -- --nocapture` remains on the accepted broad generated-reference baseline after the scoped refresh; this slice's focused smoke guard and reference-refresh command both passed
    - `scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-parser-vm-v2-native-amigaos-68020-implementation-plan-v0_1.md` passed
    - `cargo clippy --all-targets --all-features -- -D warnings` passed
    - `cargo audit --no-fetch` completed with the two existing allowed advisories for `registry` and `rand`
    - `git diff --check` passed after trimming trailing whitespace from the regenerated `prvm_smoke.lst` listing
    - `cargo test --workspace` retained the previously accepted broad baseline exception: the `asm` crate reported `866 passed; 1 failed`, with the only failure in `tests::examples_match_reference_outputs` after the intentional `prvm_smoke` reference refresh was retained and unrelated generated references were pruned
  - Current sub-slice 5b remaining work before closing this boundary slice:
    - complete; committed as `b62e1fad Exercise native PRVM label smoke parity`
  - Current item-level completion progress:
    - broadened the native interpreter from the prior straight-line smoke program to the default Rust v2 statement-program control path for covered instruction statements
    - implemented native `Jump`, `JumpIfFalse`, `IsEol`, `PeekKind`, `PeekAssignmentOperator`, and `PeekStarOrg` handling needed to route the default v2 program to the generic instruction branch without silently parsing deferred assignment/directive shapes as supported
    - implemented native checkpoint-family state management for `Checkpoint`, `Rollback`, and `Commit`, including cursor, result-count, result-byte, operand-count, finished, label, and predicate state snapshots
    - switched the AmigaOS `prvm_smoke` parser program to the default v2 statement bytecode while preserving host-mediated expression evaluation for `start: LDA #42`
    - kept deferred assignment/directive opcode families deterministic by leaving unsupported output opcodes on the existing native invalid/unsupported status paths rather than adding partial AST emission
    - refreshed only the scoped `prvm_smoke` hunk/listing references and pruned unrelated generated reference churn
  - Current item-level validation evidence:
    - `cargo test -p asm motorola68020_prvm -- --nocapture` passed, including the focused native interpreter and smoke assembly guards
    - `cargo test -p vm native_prvm_abi -- --nocapture` passed, including 10 native ABI bridge tests
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture` passed, including 2 unit tests and 7 WI-6 parity integration tests
    - `opForge_UPDATE_REFERENCE=1 cargo test -p asm examples_match_reference_outputs -- --nocapture` passed and refreshed the intentional `prvm_smoke` reference drift; unrelated `tkpkg_debug_cli` and `tokvm_interpreter` reference drift was pruned
    - `cargo test -p asm examples_match_reference_outputs -- --nocapture` remains on the accepted broad generated-reference baseline after the scoped refresh; the retained PRVM smoke references are validated by the focused smoke guard and update-mode reference pass
    - `cargo fmt --all` passed
    - `cargo clippy --all-targets --all-features -- -D warnings` passed
    - `cargo audit --no-fetch` completed with the two existing allowed advisories for `registry` and `rand`
    - `git diff --check` passed after normalizing trailing whitespace in the regenerated `prvm_smoke.lst` reference listing
    - `cargo test --workspace` retained the previously accepted broad baseline exception: the `asm` crate reported `866 passed; 1 failed`, with the only failure in `tests::examples_match_reference_outputs`
  - Expected files:
    - `examples/motorola68000/amigaos/prvm/prvm_interpreter.asm`
    - focused native PRVM tests and fixtures
    - `crates/opforge-vm/tests/parser_vm_v2_parity.rs` only if shared corpus metadata is needed
  - Validation details:
    - focused native PRVM parity test over instruction statements, data directives, assignments, block heads/tails, malformed/trailing diagnostics, checkpoint boundary, and m68k authority cases covered by WI-6 where native scope supports them
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a parity-hardening slice that broadens native PRVM only to Rust v2-authorized shapes and records unsupported deferred cases explicitly
  - Commit outcome:
    - native PRVM parity is locked against the Rust v2 authority corpus for the covered statement surface, with unsupported cases failing deterministically instead of silently diverging
  - Definition of done:
    - native PRVM handles the opcode families required by the covered WI-6 corpus
    - Rust v2 remains the oracle for every covered line and diagnostic
    - m68k special forms are validated against v2-authority behavior rather than generic host parser fallback
    - checkpoint, rollback, and output-buffer overflow paths are covered by focused tests
    - any intentionally deferred directive or statement shape is documented as unsupported with deterministic native status/diagnostic behavior

- [x] Work item 6: add the first optional AmigaOS PRVM demo harness after parity is stable
  - Source requirement or finding IDs: tokenizer-native harness sequencing pattern; completed native PRVM parity from Work item 5.
  - Validation: see the focused harness/report tests and full quality gates listed below.
  - Definition of done: see detailed criteria below for this work item.
  - Expected files:
    - `examples/motorola68000/amigaos/prvm/prvm_interpreter.asm` or a separate `prvm_debug_cli.asm`
    - `examples/reference/motorola68000/amigaos/` reference artifacts if the example enters the reference workflow
    - host-side report rendering tests for the `OPFORGE-PRVM 1` or ABI-defined report format
  - Validation details:
    - focused AmigaOS PRVM harness assembly/reference test
    - focused report-rendering tests for success, diagnostic, expression-request failure, and newline rejection
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
    - optional FS-UAE smoke only when the environment is configured
  - Current item-level completion progress:
    - added `examples/motorola68000/amigaos/prvm/prvm_debug_cli.asm` as the first optional AmigaOS-visible PRVM report harness after Work item 5 parity was committed
    - kept the harness single-line and delegated-statement scoped with embedded `start: NOP` input, the default PRVM v2 statement bytecode, and no native expression parser or instruction encoder
    - routed the harness through the existing `prvm_run_68000` native interpreter entry point and caller-owned ABI buffers
    - emitted deterministic `OPFORGE-PRVM 1` success/failure report text from ABI status, cursor, result-count, result-byte, and result-record data
    - added host-side PRVM report rendering tests for success, diagnostic, expression-request, and newline-rejection records
    - added a focused `prvm_debug_cli` assembly/payload guard and wired only the scoped `prvm_debug_cli` hunk/listing references into the example reference workflow
  - Current item-level validation evidence:
    - `cargo test -p asm motorola68020_prvm -- --nocapture` passed after the final report-helper refactor, including the new `prvm_debug_cli` assembly/payload guard and the four `OPFORGE-PRVM 1` report rendering tests
    - `opForge_UPDATE_REFERENCE=1 cargo test -p asm examples_match_reference_outputs -- --nocapture` passed and generated the scoped `prvm_debug_cli` hunk/listing references; unrelated generated reference drift was pruned
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture` passed, including 2 unit tests and 7 parity integration tests
    - `cargo fmt --all` passed after the final report-helper refactor
    - `cargo clippy --all-targets --all-features -- -D warnings` passed after replacing the eight-argument report helper with `PrvmNativeReportInput`
    - `cargo audit --no-fetch` completed with the two existing allowed advisories for `registry` and `rand`
    - `git diff --check` passed
    - `cargo test -p asm examples_match_reference_outputs -- --nocapture` and `cargo test --workspace` retained the previously accepted broad generated-reference baseline exception: the only workspace failure is `tests::examples_match_reference_outputs`; the focused PRVM guard and update-mode reference run validate this slice's scoped references
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returned `PASS` for the Work item 6 boundary slice limited to the optional single-line AmigaOS PRVM report harness, host-side `OPFORGE-PRVM 1` report rendering tests, scoped `prvm_debug_cli` references, and plan evidence; the only residual risk is the standing accepted broad `examples_match_reference_outputs` baseline exception
  - Commit outcome:
    - a first AmigaOS-visible native PRVM demo/report path exists for the already-parity-locked interpreter without making emulator execution a default dependency
  - Definition of done:
    - the harness remains single-line and delegated-statement scoped
    - CLI/file I/O behavior is deterministic and narrow, following the tokenizer harness precedent where applicable
    - report rendering uses the ABI-defined status/result records and does not invent a separate parser result contract
    - nonzero failures still produce deterministic reports when an output handle is available
    - no full assembler pass, native expression parser, or native instruction encoder is introduced

- [x] Work item 7: add the first native opcore line-routing adapter over one logical line
  - Source requirement or finding IDs: first recommended post-baseline step after Work item 6; existing Rust opcore `ProcessingRequestKind::Processor { processor: "asm", kind: "statement" }` delegation contract; tokenizer-native single-line ABI; completed PRVM single-line ABI and report harness.
  - Validation: see the focused line-router tests and full quality gates listed below.
  - Definition of done: see detailed criteria below for this work item.
  - Expected files:
    - a narrow native line-router adapter or harness source under the existing Motorola 68000/AmigaOS example or native-support tree
    - focused host-side tests that build a one-line tokenizer output, route it to `prvm_run_68000`, and compare decoded output against Rust PRVM v2 behavior
    - scoped reference artifacts only if the line-router example enters the reference workflow
    - this plan ledger
  - Validation details:
    - focused line-routing test for one newline-free statement that produces the same decoded statement/report shape as the current PRVM single-line path
    - focused rejection test for unsupported processor/kind, missing tokenizer output, or newline-containing source at the line-router boundary
    - focused preservation test showing expression operands still use the existing host-mediated expression request/resume path rather than native expression parsing
    - `cargo test -p asm motorola68020_prvm -- --nocapture`
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
  - Current item-level completion progress:
    - added `examples/motorola68000/amigaos/prvm/prvm_line_router.asm` as an import-only native AmigaOS/Motorola 68020 line-router adapter over the existing `prvm_run_68000` PRVM statement interpreter
    - defined a narrow `prvm_route_line_68000` route-frame ABI that accepts one caller-owned logical line plus tokenizer, lexeme, PRVM output, diagnostic, resume, and expression-request buffers
    - validated the route frame magic/version/size, delegated only `processor: "asm", kind: "statement"`, rejected newline-containing source before PRVM entry, and returned deterministic status values for invalid arguments, unsupported routes, and newline rejection
    - copied the route frame into the existing PRVM request frame shape and called `prvm_run_68000` without changing PRVM bytecode, expression handling, instruction encoding, macro expansion, module execution, or symbol resolution
    - added host-side report tests for delegated statement output, unsupported-route rejection, newline rejection, and expression-request pass-through
    - added a focused source-surface guard plus an actual module parse/import guard for `prvm_line_router.asm`, fixed the import-only module terminator, and reserved the internal PRVM request frame with the BSS-valid `.res byte, PRVM_REQUEST_FRAME_SIZE` form
    - excluded `prvm_line_router.asm` from the broad example reference sweep because it is an import-only module like `prvm_interpreter.asm`
  - Current item-level validation evidence:
    - `cargo fmt --all && cargo test -p asm motorola68020_prvm_line_router -- --nocapture` passed after the compliance remediation: 6 passed, 0 failed, 872 filtered out, including the actual `prvm_line_router.asm` parse/import guard
    - `cargo test -p asm motorola68020_prvm -- --nocapture` passed after the line-router parse/import guard: 13 passed, 0 failed, 865 filtered out
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture` passed, including 2 unit tests and 7 parity integration tests
    - `git diff --check` passed
    - `cargo clippy --all-targets --all-features -- -D warnings` passed
    - `cargo audit --no-fetch` completed with the two existing allowed advisories for `registry` and `rand`
    - `cargo test -p asm examples_match_reference_outputs -- --nocapture` retained the previously accepted broad generated-reference baseline exception: the only filtered test failed as `tests::examples_match_reference_outputs`, with no PRVM line-router-specific failure surfaced
    - `cargo test --workspace` retained the same accepted broad generated-reference baseline exception: 877 passed, 1 failed (`tests::examples_match_reference_outputs`) in the `asm` crate
  - Plan-compliance review evidence:
    - initial `plan-compliance-reviewer` pass returned `FAIL` because `prvm_line_router.asm` was not yet parse-valid, lacked `.endmodule`, used invalid BSS `.fill` reservation syntax, and had only synthetic/string-scan validation for the native boundary
    - remediation added the missing `.endmodule`, changed the BSS reservation to `.res byte, PRVM_REQUEST_FRAME_SIZE`, and added an actual module parse/import guard
    - second `plan-compliance-reviewer` pass returned `PASS` for the Work item 7 boundary slice limited to one newline-free native line-router adapter, focused host-side route/report tests, actual module parse/import validation, and plan evidence; Work item 8 whole-file iteration remains blocked until this Work item 7 slice is committed
  - Commit outcome:
    - one native opcore-style delegated line can be routed through tokenizer output into the parity-locked PRVM statement path with deterministic status/report behavior
  - Definition of done:
    - the adapter accepts exactly one logical newline-free line or pre-tokenized equivalent and rejects newline-containing whole-file input at this boundary
    - the adapter delegates only `processor: "asm", kind: "statement"` to PRVM; unsupported processor/kind combinations fail deterministically
    - tokenizer output and lexeme buffers remain caller-owned and ABI-bounded
    - expression handling remains Rust/opcore host-mediated through the existing pause/resume protocol
    - no CPU-family statement semantics, native expression parser, macro expansion, symbol resolution, instruction encoder, or full assembler pass is introduced

- [ ] Work item 8: add the first tokenizer/parser whole-file line iterator over the line router
  - Source requirement or finding IDs: second recommended post-baseline step after line routing; completed Work item 7 line-router adapter; existing tokenizer-native stream precedent; current blocking rule that the PRVM ABI itself remains newline-free.
  - Validation: see the focused whole-file iterator tests and full quality gates listed below.
  - Definition of done: see detailed criteria below for this work item.
  - Expected files:
    - a narrow whole-file iterator wrapper or harness source that splits input into logical lines and calls the Work item 7 line router per line
    - focused host-side tests for deterministic line splitting, per-line routing, error aggregation, and continued expression request/resume delegation
    - scoped reference artifacts only if the iterator example enters the reference workflow
    - this plan ledger
  - Validation details:
    - focused iterator test for a two-line input where each line is routed independently and output order is deterministic
    - focused newline handling tests for LF, CRLF, trailing final line without newline, skipped blank lines, and line-number preservation in diagnostics
    - focused failure test proving one failing line yields a deterministic fail-fast aggregate report and no later lines are routed after the first failure
    - focused guard proving each PRVM call still receives one newline-free logical line and caller-owned buffers
    - `cargo test -p asm motorola68020_prvm -- --nocapture`
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a whole-file-iterator-only slice that wraps the Work item 7 line router while preserving the newline-free PRVM ABI and avoiding full assembler behavior
  - Commit outcome:
    - a first native tokenizer/parser whole-file iteration path exists that processes input as ordered newline-free logical lines through the line router, without becoming macro expansion, module execution, symbol resolution, or instruction encoding
  - Definition of done:
    - whole-file input is accepted only by the iterator wrapper; `prvm_run_68000` and the line-router call remain newline-free
    - line splitting, blank-line handling, line-number accounting, and aggregate status/report policy are deterministic and covered by focused tests
    - blank logical lines are skipped without calling PRVM while still advancing source line accounting
    - aggregate failure behavior is fail-fast: the iterator records the first failing line/report and does not route later lines in that input
    - each line uses the existing tokenizer output and PRVM ABI buffers with explicit bounds checks
    - expression handling remains Rust/opcore host-mediated through the existing pause/resume protocol
    - the iterator does not implement macro expansion, module graph execution, symbol resolution, instruction encoding, output-file generation, or a native production expression parser

## Milestones

- [x] Milestone 1: native PRVM ABI/spec authority exists and passes quality review (`Work item 1`).
- [x] Milestone 2: host-side ABI decode and parity fixtures exist before native assembly lands (`Work item 2`).
- [x] Milestone 3: `prvm_run_68000` can execute one delegated newline-free statement path over caller-owned buffers (`Work item 3`).
- [x] Milestone 3a: the first native PRVM slice has a minimal opt-in FS-UAE smoke executable before expression work begins (`Work item 3a`).
- [x] Milestone 4: native PRVM expression operand parsing works through host-mediated Rust/opcore sub-calls (`Work item 4`).
- [x] Milestone 5: native PRVM parity is broadened to the WI-6 Rust v2 authority corpus (`Work item 5`).
- [x] Milestone 6: an optional AmigaOS demo/report harness exists only after parity is stable (`Work item 6`).
- [x] Milestone 7: one native opcore-style logical line routes through tokenizer output into PRVM without broadening parser semantics (`Work item 7`).
- [ ] Milestone 8: whole-file input is iterated as deterministic newline-free logical lines over the line router (`Work item 8`).

## To Be Spec'd / Planned Later

- native opcore expression parser
- native macro expansion, module graph execution, symbol resolution, or full assembler pass loop
- native instruction selector/encoder VM execution
- emulator-required CI gates
- final production CLI UX for a complete native opForge assembler

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no plan activation before `plan-quality-reviewer` or `plan-quality-orchestrator` returns `PASS`
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- do not start native assembly before the native PRVM ABI/spec from Work item 1 passes its validation
- do not implement or embed production native expression parsing; use only the host-mediated Rust/opcore sub-call protocol, except for the smoke-only one-token literal pass-back shim allowed above
- do not parse raw whole-file input or newline-containing source in the first native PRVM ABI
- do not start Work item 8 whole-file iteration until Work item 7 line routing is committed and plan-compliance has passed
- whole-file iteration may split input into logical lines only in its wrapper; `prvm_run_68000` and the line-router call must remain newline-free and caller-buffer bounded
- do not move CPU-family statement semantics into native interpreter branches; keep specialization in packages and PRVM bytecode
- do not add the AmigaOS CLI/file I/O harness before host-side native PRVM parity is stable