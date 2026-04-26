# opForge Tokenizer VM Stream + Native AmigaOS 68020-Baseline Implementation Plan v0.1

## Metadata

- Source: `User request on 2026-04-13`, current tokenizer VM authority path in `documentation/opforge-assembler-vm-path-guide-v0_1.md`, the AmigaOS tokenizer harness contract in `documentation/opForge-amigaos-cli-fileio-tokenizer-vm-harness-spec-v0_1.md`, the native single-line tokenizer buffer ABI authority in `documentation/opForge-m68000-tokenizer-vm-single-line-buffer-abi-spec-v0_1.md`, and review findings in `documentation/reviews/opforge-tokenizer-vm-stream-native-68000-implementation-plan-review-2026-04-13.md`
- Mode: `implementation`
- Owner: Codex

## Objective

Advance the tokenizer VM from its current partially host-delegated form to a
real VM-owned tokenizer that can consume bytes from an abstract input stream,
then use that same contract to land the first native AmigaOS 68020-baseline
tokenizer slice and wrap it in the already-specified AmigaOS CLI/file I/O
harness.

The first native and harness slices remain explicitly single-line only. They do
not attempt whole-file tokenization in one native call, refillable native input
streams, or implicit multi-line iteration.

The execution target is intentionally narrow: tokenizer VM only. This plan does
not attempt native parser VM, native expression VM, native instruction
encoding, or a native full-assembler pass loop.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Scope is limited to tokenizer VM stream input, explicit VM-owned token
  recognition, a first native AmigaOS 68020-baseline tokenizer interpreter
  slice, and the tokenizer-specific AmigaOS CLI/file I/O harness already
  defined by spec.
- The plan must not broaden into preprocessor, macro-expansion, module-graph,
  parser VM, expression VM, selector VM, or native whole-assembler execution.
- The host-side assembler integration remains line-oriented for this plan even
  if the tokenizer VM consumes bytes through an abstract stream contract.
- The current package-driven token policy remains authoritative; execution must
  not hard-code family behavior in a way that diverges from package/runtime
  ownership.
- Any 68000-specific tokenizer behavior, bytecode shape, token policy,
  character-class mapping, or state-machine specialization must live only in
  package-owned tokenizer VM descriptors or package artifacts, not in ad hoc
  runtime branches, native interpreter special cases, or harness-side parsing
  logic.
- The current Hunk output path and AmigaOS example executables must not be
  regressed while tokenizer-native work lands.
- FS-UAE remains opt-in validation only and must not become a default required
  dependency for local or CI quality gates.
- This plan must not become active until `plan-quality-reviewer` returns
  `PASS`.
- One active work item at a time.
- Each work item or phase must end in exactly one new commit before the next
  item begins.
- Full quality gates are mandatory before each commit.
- `plan-compliance-reviewer` must pass before each plan-driven commit.
- The first native AmigaOS 68020-baseline interpreter slice and the first
  AmigaOS harness slice
  are single-line only; newline-containing input must be rejected
  deterministically instead of being processed as a whole file in one native
  call.
- The first native AmigaOS tokenizer implementation and harness slices target
  `.cpu 68020` as the baseline CPU while preserving the existing
  `tokvm_run_68000` entry-symbol name.
- If execution reveals unresolved behavioral questions about the tokenizer
  stream contract or native token/status buffer ABI that materially affect
  compatibility, stop and author a spec instead of silently widening this plan.

## Planning Decisions Captured Up Front

- The first production slice lands stream-contract plumbing only; host
  tokenizer delegation is removed in Work item 2 before any native 68000
  assembly implementation begins.
- The first stream contract is line-buffer-backed and byte-oriented; true
  whole-file refill behavior is deferred until the explicit line-scoped VM path
  is working.
- The runtime and native AmigaOS 68020-baseline interpreter must stay generic
  to the tokenizer VM opcode contract; 68000-specific tokenizer semantics are
  selected by the package-provided program or policy, not by interpreter-side
  behavior forks.
- The first native AmigaOS 68020-baseline slice is single-line only: one newline-free source
  line goes into `tokvm_run_68000`, and newline-containing input is rejected
  deterministically until a later spec defines multi-line iteration.
- The first native AmigaOS 68020-baseline slice owns only `tokvm_run_68000`
  over caller-provided buffers, VM bytecode, and a minimal authoritative token/status
  buffer ABI; AmigaOS CLI parsing and file I/O stay out of that commit and land afterward.
- The AmigaOS harness must reuse the existing harness spec rather than invent a
  second native tokenizer contract.

## Work Items

- [x] Work item 1: land an explicit tokenizer VM stream contract in the package and Rust runtime
  - Source requirement or finding IDs: user request for full VM tokenization from an input stream; VM path guide `4.8 Tokenization`; harness spec `Behavioral Contract` ABI ownership for `tokvm_run_68000`; `RVW-2026-04-13-003` (partial sequencing alignment)
  - Validation:
  - Definition of done:
    - the tokenizer VM runtime has an explicit byte-oriented stream model with deterministic cursor and end-of-line behavior
    - the runtime can execute tokenizer programs against that stream model without introducing parser or assembler-pass behavior changes
    - any remaining compatibility fallback is isolated and temporary rather than being the design endpoint
    - package/runtime contract tests cover the new stream-owned fields or opcodes
    - the stream contract does not introduce any 68000-specific tokenizer branch in the runtime core; family specialization remains package-owned
    - add focused tokenizer stream-contract and opcode-path coverage
    - run `cargo test -p package encode_decode_round_trip_contract_schema_tokenizer_vm_programs -- --nocapture`
    - run `cargo test -p vm execution_model_tokenizer_vm_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-package/src/package.rs`
    - `crates/opforge-vm/src/runtime_model_core.rs`
    - `crates/opforge-vm/src/runtime_portable_types.rs`
    - `crates/opforge-vm/src/tokenizer_runtime_utils.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to tokenizer stream-contract/runtime plumbing with no parser or encoder expansion
  - Commit outcome:
    - the tokenizer VM runtime exposes explicit byte-stream operations and token-emission bookkeeping needed for a self-contained tokenizer path instead of assuming host token delegation
  - Validation details:
    - repository-wide gate unblocker only: `crates/opforge-asm/src/asmline_instruction.rs` gates the local bare-symbol canonicalization test module behind `not(feature = "vm-runtime-only")` and imports `BinaryOp` so `cargo clippy --all-targets --all-features` can compile the existing tests under the repo's feature matrix without changing production behavior
    - repository-wide gate unblocker only: `examples/motorola68000/amigaos/helloworld.asm` restores explicit `SysBase.W` accesses so the checked-in example matches the committed reference payload during `cargo test --workspace`; this does not change tokenizer VM behavior

- [x] Work item 2: replace `ScanCoreToken`-driven default tokenizer programs with explicit VM tokenization logic
  - Source requirement or finding IDs: user request for full VM tokenization; VM path guide `4.8 Tokenization`; current tokenizer authority path in `crates/opforge-vm/src/execution_model/tokenizer_bridge.rs`; `RVW-2026-04-13-003` (full sequencing alignment)
  - Validation:
    - `cargo test -p vm execution_model_tokenizer_ -- --nocapture`
    - `cargo test -p vm execution_model_assembler_tokenization_path_ -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace -q`
  - Definition of done:
    - the default tokenizer VM program bytes no longer use `ScanCoreToken` or `DelegateCore`
    - token recognition for the currently authoritative tokenizer families is performed by VM logic over the explicit stream contract
    - parity and deterministic budget failures remain locked by focused tests
    - still-unsupported tokenizer shapes, if any, fail deterministically instead of silently falling back to host behavior
    - any 68000-specific tokenizer specialization added by this slice is expressed only as package-scoped VM program or token-policy data, not as runtime special cases
    - add focused parity and budget tests for explicit VM tokenization of whitespace, comments, identifiers, numbers, strings, punctuation, and operators
    - add one focused failure-path test proving unsupported tokenizer shapes fail deterministically after `ScanCoreToken` removal
    - run `cargo test -p vm execution_model_tokenizer_ -- --nocapture`
    - run `cargo test -p vm execution_model_assembler_tokenization_path_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-vm/src/builder.rs`
    - `crates/opforge-vm/src/runtime_model_core.rs`
    - `crates/opforge-vm/src/tokenizer_runtime_utils.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-package/src/package.rs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to tokenizer VM program replacement and parity locking, with no native 68000 assembly yet
  - Commit outcome:
    - the default tokenizer VM path becomes explicitly VM-owned and no longer relies on `ScanCoreToken` or any equivalent host-tokenizer escape hatch for the covered families, while any 68000-specific logic remains package-owned
  - Validation details:
    - default tokenizer VM bytecode now dispatches explicit VM-native identifier, number, string, and symbol scan opcodes instead of relying on `ScanCoreToken`
    - tokenizer lexeme accounting now uses source spans, and the default tokenizer lexeme budgets were raised from `256` to `1024` so valid long-string source lexemes still reach directive-level validation like `.ptext` length checks instead of failing early in the tokenizer

- [x] Work item 3: land the first native AmigaOS 68020-baseline tokenizer interpreter slice over caller-owned buffers
  - Source requirement or finding IDs: user request for native AmigaOS baseline focus; harness spec `Behavioral Contract` `tokvm_run_68000` ABI; harness spec `Acceptance Criteria` tokenizer VM call boundary; `documentation/opForge-m68000-tokenizer-vm-single-line-buffer-abi-spec-v0_1.md`; `RVW-2026-04-13-001` (single-line native contract alignment); `RVW-2026-04-13-002` (token/status buffer ABI prerequisite)
  - Validation:
    - `cargo test -p asm motorola68020_tokvm -- --nocapture`
    - `opForge_UPDATE_REFERENCE=1 cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
  - Definition of done:
    - before native execution behavior is treated as correct, `documentation/opForge-m68000-tokenizer-vm-single-line-buffer-abi-spec-v0_1.md` defines the single-line token record layout and return-status contract used by the first `tokvm_run_68000` slice
    - `tokvm_run_68000` exists for one single-line, line-buffer-backed tokenizer execution path on a `.cpu 68020` baseline using the call ABI already named in the harness spec
    - newline-containing input is rejected deterministically rather than being processed in one native call
    - the native AmigaOS 68020-baseline slice owns tokenizer execution only and does not absorb CLI parsing, file I/O, parser work, or encoder work
    - deterministic success and failure status are observable through that defined token-buffer and return-status contract
    - the assembled interpreter artifact is covered by a focused host-side validation path
    - the native AmigaOS 68020-baseline interpreter executes generic tokenizer VM opcodes and consumes package-provided tokenizer programs without embedding 68000-specific tokenization rules in interpreter control flow
    - add one focused host-side contract test for the documented single-line token-buffer and return-status ABI
    - add one focused host-side smoke test proving the `.cpu 68020` interpreter slice assembles and preserves that ABI for one newline-free line buffer
    - run `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture`
    - run `cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `examples/motorola68000/` or a new native tokenizer assembly fixture directory for the interpreter source
    - `examples/reference/motorola68000/` for any committed reference artifacts required by the landed slice
    - `crates/opforge-asm/src/tests.rs`
    - `documentation/opForge-m68000-tokenizer-vm-single-line-buffer-abi-spec-v0_1.md` if the spec requires a landed clarification during implementation
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the first native `tokvm_run_68000` interpreter path over caller-owned buffers, with no AmigaOS CLI/file I/O yet
  - Commit outcome:
    - a first native AmigaOS 68020-baseline tokenizer interpreter exists and can execute the explicit tokenizer VM contract over a supplied newline-free source buffer, token buffer, scratch buffer, and tokenizer bytecode program, with 68000-specific tokenizer semantics still owned by package content
  - Validation details:
    - `examples/motorola68000/amigaos/tokvm/tokvm_interpreter.asm` now provides a `.cpu 68020` `tokvm_run_68000` entrypoint, newline rejection, default-program-compatible control flow, and big-endian token-record writes for the single-line ABI slice
    - `crates/opforge-asm/src/tests.rs` now decodes fixed 20-byte native token records into the exact `OPFORGE-TOKVM 1` report shape and includes a fixture smoke test that assembles the new interpreter example and checks its ABI marker/listing surface
    - `examples/reference/motorola68000/amigaos/tokvm_interpreter.hunk` and `examples/reference/motorola68000/amigaos/tokvm_interpreter.lst` lock the assembled native interpreter artifact into the existing reference workflow without adding any AmigaOS CLI/file I/O yet

- [x] Work item 4: wrap the native tokenizer interpreter in the spec-defined AmigaOS CLI/file I/O harness
  - Source requirement or finding IDs: `documentation/opForge-amigaos-cli-fileio-tokenizer-vm-harness-spec-v0_1.md` `Goals`; `Behavioral Contract`; `Boundary Cases`; `Acceptance Criteria`; `RVW-2026-04-13-001` (single-line harness alignment)
  - Validation:
    - `cargo test -p asm motorola68020_tokvm -- --nocapture`
    - `opForge_UPDATE_REFERENCE=1 cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - `cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
  - Definition of done:
    - the reusable `amigaos_cli_fileio` support layer and tokenizer-specific harness both exist and follow the current harness spec
    - the first harness slice is single-line only: it reads one input file into the configured source buffer, rejects newline-containing input deterministically, calls `tokvm_run_68000` once for one newline-free line, and writes the `OPFORGE-TOKVM 1` report format
    - usage, quoted-path rejection when quoting is unsupported, file-open, input-too-large, VM-failure, output-open, partial-write, and output-write failures remain deterministic
    - when an output handle exists, nonzero exits still attempt to write the result report before returning failure
    - default workspace validation remains green without FS-UAE, while an opt-in emulator path exists when configured
    - the harness selects and feeds package-owned tokenizer VM content into `tokvm_run_68000` without adding harness-local 68000 tokenizer semantics, and the native AmigaOS harness code targets `.cpu 68020`
    - add focused host-side report-format tests for `OPFORGE-TOKVM 1`
    - add focused negative tests for missing arguments, newline-containing input rejection, quoted-path rejection when unsupported, input-too-large, output-open failure, and partial-write failure
    - add one focused report-emission-on-failure test for nonzero exits when an output handle exists
    - add one focused build-path test proving the harness assembles to Hunk output
    - run `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture`
    - run `cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - run the opt-in FS-UAE smoke path when the environment is configured
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - tokenizer-harness assembly sources and reusable `amigaos_cli_fileio` module under the chosen Motorola 68000 example or module path with a `.cpu 68020` baseline
    - `examples/motorola68000/amigaos/`
    - `examples/reference/motorola68000/amigaos/`
    - host-side tests or harness fixtures needed to parse `OPFORGE-TOKVM 1` reports
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the existing AmigaOS tokenizer harness spec, post-interpreter integration, and opt-in emulator validation
  - Commit outcome:
    - the first native AmigaOS 68020-baseline tokenizer VM slice is executable through the spec-defined `tokvm <input-path> <output-path>` AmigaOS harness without making FS-UAE a default quality gate dependency
  - Validation details:
    - `examples/motorola68000/amigaos/tokvm/tokvm_interpreter.asm` now wraps `tokvm_run_68000` in a `.cpu 68020` Shell/Workbench-aware entry path, reuses a narrow `amigaos_cli_fileio_*` DOS support layer, parses raw CLI arguments through `GetArgStr`, rejects quoted paths deterministically, reads one fixed source buffer plus a one-byte overflow probe, and emits `OPFORGE-TOKVM 1` reports through exact writes while still attempting report emission for nonzero VM/file failures once an output handle exists
    - `crates/opforge-asm/src/tests.rs` now covers the CLI parser policy, exact-write policy, negative report rendering for input-too-large and output-open failures, nonzero report emission shape, newline rejection report shape, and the assembled CLI harness surface for the Hunk/listing artifact
    - `examples/reference/motorola68000/amigaos/tokvm_interpreter.hunk` and `examples/reference/motorola68000/amigaos/tokvm_interpreter.lst` were refreshed against the new CLI harness output using `opForge_UPDATE_REFERENCE=1`
    - the opt-in FS-UAE smoke path was not run because no emulator environment was configured in this session, so the default non-emulator quality gate remains the recorded evidence for this slice

- [x] Work item 5: harden the native AmigaOS tokenizer harness and readable demo program after the initial landing
  - Source requirement or finding IDs: user follow-up runtime reports from 2026-04-14 for the existing tokvm AmigaOS path; `documentation/opForge-amigaos-cli-fileio-tokenizer-vm-harness-spec-v0_1.md` `Boundary Cases`; `documentation/opForge-m68000-tokenizer-vm-single-line-buffer-abi-spec-v0_1.md`; work-item-4 follow-up correctness hardening
  - Validation:
    - `cargo test -p asm motorola68020_tokvm -- --nocapture`
    - `opForge_UPDATE_REFERENCE=1 cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - `cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
  - Definition of done:
    - the existing native AmigaOS tokvm harness no longer corrupts the output handle across repeated hex writes and no longer mangles trailing CR/LF in CLI paths
    - the runtime report path validates VM outputs before formatting token metadata so invalid token records degrade to deterministic VM failure output instead of crashing or emitting malformed reports
    - the native tokenizer VM continues past the first lexeme by preserving the program counter across scan helper calls
    - the embedded demo bytecode is readable through symbolic opcode, class, and jump labels without regressing Hunk assembly output
    - focused host-side regressions and refreshed references lock the corrective behavior without widening scope beyond the existing tokvm AmigaOS path
  - Expected files:
    - `examples/motorola68000/amigaos/tokvm/tokvm_interpreter.asm`
    - `crates/opforge-asm/src/tests.rs`
    - `examples/reference/motorola68000/amigaos/tokvm_interpreter.hunk`
    - `examples/reference/motorola68000/amigaos/tokvm_interpreter.lst`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to post-landing correctness hardening of the existing AmigaOS tokvm harness and demo bytecode readability with no parser, encoder, or broader runtime expansion
  - Commit outcome:
    - the existing native AmigaOS tokvm tokenizer example remains the same narrow single-line harness but is now stable across repeated report writes, newline-terminated Shell args, continued multi-token scans, and symbolic demo-program maintenance
  - Validation details:
    - `examples/motorola68000/amigaos/tokvm/tokvm_interpreter.asm` now preserves `D1` in `amigaos_cli_fileio_write_exact`, trims LF/CR in the CLI parser, validates VM results before report formatting, uses the stored `demoProgramLen` value, zero-extends live input bytes in `tokvmOpcodeReadChar`, restores `A0` after scan helper calls, and rewrites `demoProgram` with symbolic class/opcode/jump labels while keeping the `.output "build/tokvm"` Hunk path
    - `crates/opforge-asm/src/tests.rs` now covers the trailing-newline CLI parser case, exact-write handle preservation, CR/LF argument termination, restored program-counter control flow, demo-program readability, and the existing tokvm reference-workflow path including the `build/tokvm` payload name
    - `examples/reference/motorola68000/amigaos/tokvm_interpreter.hunk` and `examples/reference/motorola68000/amigaos/tokvm_interpreter.lst` were refreshed to lock the corrected native tokvm output shape

## Milestones

- [x] Milestone 1: the tokenizer VM exposes an explicit byte-stream contract in package/runtime without widening into parser or encoder work (`Work item 1`)
- [x] Milestone 2: default tokenizer execution becomes explicitly VM-owned and no longer depends on host token scanning (`Work item 2`)
- [x] Milestone 3: a first native AmigaOS 68020-baseline tokenizer interpreter executes the tokenizer VM contract over caller-owned single-line buffers while keeping 68000-specific tokenizer logic package-owned (`Work item 3`)
- [x] Milestone 4: the native AmigaOS 68020-baseline interpreter is runnable through a first single-line AmigaOS CLI/file I/O tokenizer harness without widening the default quality gate (`Work item 4`)
- [x] Milestone 5: the initial native AmigaOS tokvm harness path is hardened against post-landing runtime and maintenance failures without broadening scope beyond the existing tokenizer example (`Work item 5`)

## To Be Spec’d / Planned Later

- true refillable whole-file tokenizer streams beyond the first line-buffer-backed contract
- multi-line file iteration for the AmigaOS tokenizer harness beyond the first single-line native call path
- native parser VM
- native expression VM
- native instruction selector/encoder VM
- native preprocessing, macro expansion, or module-graph execution
- emulator-driven differential testing beyond the first opt-in tokenizer harness slice

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no plan activation before `plan-quality-reviewer` returns `PASS`
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- do not start Work item 3 implementation until the single-line token-buffer and return-status ABI in `documentation/opForge-m68000-tokenizer-vm-single-line-buffer-abi-spec-v0_1.md` remains the active authority for the first native slice
- do not widen this plan into native full-assembler work, parser/expression VM work, or non-tokenizer AmigaOS runtime expansion
- do not reactivate host tokenizer delegation once Work item 2 lands
- do not treat newline-containing input as supported native tokenizer input in Work items 3 or 4; defer multi-line processing to a later spec or plan item
- do not move 68000-specific tokenizer logic into runtime-core branches, native interpreter forks, or AmigaOS harness parsing paths; keep that specialization in packages only
