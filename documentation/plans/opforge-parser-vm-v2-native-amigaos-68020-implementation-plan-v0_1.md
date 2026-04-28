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
"statement" }`. It does not replace opcore line routing, does not port the
opcore expression parser, and does not become a whole-file assembler pass.

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
- Expression parsing remains Rust/opcore-owned. Native PRVM requests expression
  parsing only through a host-mediated sub-call protocol over explicit operand
  token ranges.
- No native expression parser, native instruction encoder, macro expander,
  module graph, or full assembler pass may be introduced by this plan.
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

- [ ] Work item 4: implement host-mediated opcore expression sub-calls for native PRVM
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
    - add the host bridge path that turns native expression requests into actual Rust/opcore expression parses and writes the resulting expression slot back to native buffers
    - add multi-operand native pause/resume coverage driven by host-owned expression slots
    - preserve `Expr::Error` parity through the host bridge, not only through ABI decode tests
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
  - Commit outcome:
    - native PRVM can request Rust/opcore expression parsing over explicit operand token ranges and resume with returned expression slots while preserving Rust PRVM v2 AST and `Expr::Error` behavior
  - Definition of done:
    - native PRVM returns the ABI-defined expression-request status rather than attempting to parse expressions locally
    - the host bridge invokes the existing Rust opcore expression parser for the requested token range
    - returned expression slots are inserted into the decoded native parser result in operand order
    - expression errors are preserved in the same normalized shape as Rust PRVM v2 for the covered cases
    - multiple expression sub-calls in one statement remain deterministic and bounded

- [ ] Work item 5: broaden native PRVM opcode and statement parity to the WI-6 authority corpus
  - Source requirement or finding IDs: WI-6 `parser_vm_v2_parity` corpus; Rust PRVM v2 source-of-truth requirement; native-port readiness gate.
  - Validation: see the focused native parity corpus tests and full quality gates listed below.
  - Definition of done: see detailed criteria below for this work item.
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

- [ ] Work item 6: add the first optional AmigaOS PRVM demo harness after parity is stable
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
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a harness-only slice that does not broaden native parser semantics beyond the parity-locked interpreter
  - Commit outcome:
    - a first AmigaOS-visible native PRVM demo/report path exists for the already-parity-locked interpreter without making emulator execution a default dependency
  - Definition of done:
    - the harness remains single-line and delegated-statement scoped
    - CLI/file I/O behavior is deterministic and narrow, following the tokenizer harness precedent where applicable
    - report rendering uses the ABI-defined status/result records and does not invent a separate parser result contract
    - nonzero failures still produce deterministic reports when an output handle is available
    - no full assembler pass, native expression parser, or native instruction encoder is introduced

## Milestones

- [x] Milestone 1: native PRVM ABI/spec authority exists and passes quality review (`Work item 1`).
- [x] Milestone 2: host-side ABI decode and parity fixtures exist before native assembly lands (`Work item 2`).
- [x] Milestone 3: `prvm_run_68000` can execute one delegated newline-free statement path over caller-owned buffers (`Work item 3`).
- [x] Milestone 3a: the first native PRVM slice has a minimal opt-in FS-UAE smoke executable before expression work begins (`Work item 3a`).
- [ ] Milestone 4: native PRVM expression operand parsing works through host-mediated Rust/opcore sub-calls (`Work item 4`).
- [ ] Milestone 5: native PRVM parity is broadened to the WI-6 Rust v2 authority corpus (`Work item 5`).
- [ ] Milestone 6: an optional AmigaOS demo/report harness exists only after parity is stable (`Work item 6`).

## To Be Spec'd / Planned Later

- native opcore line routing
- native opcore expression parser
- native tokenizer/parser whole-file iteration in one call
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
- do not implement or embed native expression parsing; use only the host-mediated Rust/opcore sub-call protocol
- do not parse raw whole-file input or newline-containing source in the first native PRVM ABI
- do not move CPU-family statement semantics into native interpreter branches; keep specialization in packages and PRVM bytecode
- do not add the AmigaOS CLI/file I/O harness before host-side native PRVM parity is stable