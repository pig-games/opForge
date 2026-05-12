# libopforge Developer Guide

This guide is for developers who want to embed opForge as a library, build new tools on top of it, or integrate it into an existing build/editor/runtime environment.

It documents the current published `libopforge` host surface in this
branch/worktree as of `v0.9.7`.

> Status note: `libopforge` `v0.9.7` is a published pre-1.0 preview API. It is
> the recommended current integration path, but names, signatures, module
> boundaries, defaults, and behavior may still change across future `0.x`
> releases.

## 1. Integration map

Common integration starting points:

| Use case | Start with | Why |
|---|---|---|
| Rust CLI, build tool, GUI, server | `libopforge::asm` | This is the recommended high-level embedding surface today |
| In-memory editor, browser-like host, tests | `libopforge::asm` + `libopforge::io` | Swap filesystem I/O for memory-backed providers/sinks |
| Background validation or repeated builds | `AssemblerSession::builder(...)`, `prepare()`, `check()` | These separate ownership, preparation, and execution cleanly while keeping the host entry path builder-first |
| Syntax-aware editor features | `libopforge::opcore`, `libopforge::processing`, `libopforge::asm::opasm` | These expose tokenization, expression parsing, line routing, and statement processing |
| CPU discovery or support UI | `libopforge::registry` | Introspect builtin CPUs/families/dialects without touching internals |
| Formatter hosts | `libopforge::formatter` | Access the current formatter surface without depending on lower-level crates |
| C or C++ host integration | `crates/opforge-ffi` | Thin ABI layer over the same library/session model |
| New CPU/family/dialect implementations | direct workspace crates (`registry`, `families`, `vm`) | This is advanced extension work, not the normal preview embedding path |

External Rust consumers should depend on `libopforge` and stay inside its module tree unless they are intentionally extending opForge itself.

## 2. Workspace map

The workspace is deliberately layered.

- `crates/opforge-lib` (package name `libopforge`) is the published facade crate. It defines the current module layout and is the crate downstream Rust code should import for the pre-1.0 host surface.
- the facade remains curated and host-facing. It defines the current module groups, config types, session types, and high-level helpers directly from the crate-owned layout.
- `crates/opforge-engine` owns orchestration: source loading, preprocessing, module graph expansion, registry bootstrap, output routing, runtime model loading, and lockstep coordination.
- `crates/opforge-core` (`opcore`) owns non-assembler language semantics such as tokenization, expressions, module-item handling, macro processing, and scopes.
- `crates/opforge-asm` owns assembler-specific behavior: statement parsing, evaluation, encoding, listings, output payloads, and assembler diagnostics/reporting.
- `crates/opforge-vm` owns the shared VM/runtime/package machinery and the portable contracts used by VM-backed and portable tooling paths.
- `crates/opforge-registry` and `crates/opforge-families` own CPU/family/dialect registration plus builtin architecture behavior.
- `crates/opforge-formatter`, `crates/opforge-lsp`, `crates/opforge-cli-core`, `crates/opforge-cli`, and `crates/opforge-ffi` are host/tool layers built on top of the split library.

The practical mental model is:

1. `opcore` decides whether a line is generic language structure or must be handed to another processor.
2. `asm` handles assembler statements and output-oriented execution.
3. `engine` ties source expansion, registry lookup, execution mode, and output sinks together.
4. `libopforge` presents the current documented host boundary over those pieces.

## 3. Current published API boundary

The root crate intentionally exposes a module-first API:

- `libopforge::asm`
- `libopforge::asm::opasm`
- `libopforge::opcore`
- `libopforge::diagnostics`
- `libopforge::io`
- `libopforge::processing`
- `libopforge::registry`
- `libopforge::lockstep`
- `libopforge::formatter`

Important boundary notes:

- The normal recommended Rust embedding path is `libopforge::asm::Assembler` or `libopforge::asm::AssemblerSession`.
- The `libopforge` package in `crates/opforge-lib` is the public facade. External consumers should target that crate rather than the lower-level workspace crates directly.
- The registry module in the preview facade is primarily for lookup and introspection. Full custom family/CPU registration is still an advanced lower-level workflow in the workspace crates.

### 3.1 Concern inventories

Each public module has a distinct ownership boundary.

| Module | Owns | Does not own |
|---|---|---|
| `libopforge::asm` | High-level assembly embedding, grouped config and session lifecycle, assembler workflow errors, and output-oriented assembly helpers | Generic language parsing, processor-neutral routing, or CPU discovery as primary concerns |
| `libopforge::asm::opasm` | CPU-aware statement tokenization, parsing, processing, and portable statement forms without full assembly | Full assembly orchestration, artifact emission, or registry discovery |
| `libopforge::opcore` | Generic non-assembler language services: tokenization, expressions, module items, macros, preprocess, and `CoreError` | Assembler statement encoding, listings, or artifact output |
| `libopforge::processing` | Processor-neutral routing, processing traces, neutral processor failures, and editor-style line dispatch | High-level assembler workflow packaging or generic language ownership |
| `libopforge::diagnostics` | Current assembler diagnostics, run reports, and assembler diagnostic taxonomy | Source loading, registry discovery, or execution-mode policy |
| `libopforge::io` | Current filesystem and memory-backed source and output adapters | Diagnostics taxonomy, CPU selection, or assembly semantics |
| `libopforge::registry` | CPU, family, and capability lookup plus builtin registry introspection | Full custom extension authoring as a preview-facade concern |
| `libopforge::lockstep` | Execution-head selection, lockstep checkpoints, and parity reporting | Statement parsing, assembly workflow, or registry discovery |
| `libopforge::formatter` | Current formatter configuration, formatter runs, and formatter reports | Reclassifying assembler or opcore diagnostics |

CLI or host presentation may specialize wording more than lower API layers do.
The current library boundary preserves structured categories, codes, and
ownership boundaries even when higher-level tools choose different user-facing
phrasing.

## 4. Assembly lifecycle

All high-level assembly flows share the same stages:

1. Choose a root source path and an `output_base`.
2. Resolve source input through either the filesystem or a custom `SourceProvider`.
3. Expand preprocessor directives, includes, and module graph dependencies.
4. Resolve the target CPU through the builtin registry, optionally using `cpu_override`.
5. Run the assembler in `Rust`, `Vm`, or `Lockstep` mode.
6. Emit outputs through either the filesystem or a custom `OutputSink`.
7. Consume `AsmRunReport` diagnostics and, when needed, prepared-session metadata such as `SourceMap` and dependency files.

### 4.1 `output_base` matters

`output_base` is not cosmetic. It drives output naming when you use default outputs such as listing and hex files.

In the current Rust API, when `output_base` is omitted, the public `assemble()`
and `prepare()` paths derive it from `root_path` by removing the source
extension. If your host wants artifact names that differ from that
path-derived default, set `output_base` explicitly.

### 4.2 Execution mode defaults

The default execution mode in the current config types is `ExecutionMode::Vm`.

Use:

- `ExecutionMode::Vm` for the normal current default path.
- `ExecutionMode::Rust` when you want the native Rust continuation head.
- `ExecutionMode::Lockstep { continuation_head: ... }` when validating Rust/VM parity and consuming a `LockstepReport`.

Reference example:

- `documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs`

Detailed execution-mode and lockstep behavior is documented in `documentation/libopforge-execution-modes-and-lockstep-guide.md`.

### 4.3 `check()` versus `assemble()`

Use `check()` when you want validation without output-side effects.

In the current API, `check()` explicitly normalizes the output configuration so
it disables default outputs, labels, dependency output, binary specs, and
output-file overrides before assembling. Internally that means forcing
`no_outputs = true`, which makes `check()` the right background-validation call
for editor tooling and CI-style validation.

### 4.4 `prepare()` versus one-shot execution

Use a prepared flow when you need the boundary between expansion/preparation and final emission:

- inspect `root_module_id()`
- inspect `cpu_name()`
- inspect `source_map()`
- inspect `dependency_files()`
- reuse the prepared state for later `assemble()` or `check()`

This is especially useful in tools that want to present dependency/source-map metadata before deciding whether to emit artifacts.

Contract split:

- `Assembler::prepare()` and `AssemblerSession::prepare()` preserve the full grouped config needed for later prepared execution.
- the free `prepare()` helper supports preparation inputs plus `output_base` and `execution_mode`; when omitted, `execution_mode` still defaults explicitly to `Vm`.
- if you need a prepared flow with custom sinks, output overrides, or broader reusable output configuration, prefer `Assembler` or `AssemblerSession`.

Reference example:

- `documentation/libopforge-developer-guide-examples/libopforge_prepared.rs`

## 5. High-level Rust integration patterns

Concrete embedding recipes are collected in `documentation/libopforge-embedding-cookbook.md`.

### 5.1 Borrowed host integration

Use `Assembler::builder(...)` when your host already owns its providers and sinks and can keep them alive for the duration of the call. This remains the best fit for short-lived build-tool or command-wrapper requests.

A borrowed-host example is in `documentation/libopforge-developer-guide-examples/libopforge_borrowed.rs`. Additional borrowed/session setup examples are in `documentation/libopforge-embedding-cookbook.md`.

### 5.2 Owned or non-borrowing host integration

Use `AssemblerSession::builder(...)` when your host needs owned state, long-lived sessions, or FFI-friendly ergonomics. This is still the recommended default for new integrations.

Additional owned/session setup examples are in `documentation/libopforge-embedding-cookbook.md`.

Reference examples:

- `documentation/libopforge-developer-guide-examples/libopforge_borrowed.rs`
- `documentation/libopforge-developer-guide-examples/libopforge_in_memory.rs`
- `documentation/libopforge-developer-guide-examples/libopforge_filesystem.rs`
- `documentation/libopforge-developer-guide-examples/libopforge_prepared.rs`
- `documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs`

### 5.3 In-memory integration

For editors, tests, and embedded hosts, use the concrete memory adapters re-exported from `libopforge::io`.

The in-memory recipe, including `MemoryOutputSink::bytes()` versus `text()`, is documented in `documentation/libopforge-embedding-cookbook.md`.

### 5.4 Reusing prepared state

If your tool needs published metadata before deciding whether to emit outputs,
use a prepared session and treat that prepared boundary as the reusable handoff
point for source-map, CPU, and dependency data.

Prepared-session examples are in `documentation/libopforge-embedding-cookbook.md`. Runtime-mode details for prepared flows are in `documentation/libopforge-execution-modes-and-lockstep-guide.md`.

## 6. Tooling-oriented lower-level APIs

Not every tool needs full assembly. The preview facade also exposes lower-level
services for syntax-aware tooling.

### 6.1 `libopforge::opcore`

Use `opcore` when you need generic language services that are not tied to full assembly:

- `tokenize_line`
- `parse_expression`
- `parse_expression_tokens`
- `process_module_item`

This is a good fit for syntax highlighting, expression-aware UIs, and quick validation of `.use`, `.module`, and related module-item forms.

The `portable` submodule exposes portable token and AST structures derived from the VM portable contract. It is useful for tools that want a serialization-friendly or FFI-friendly view rather than Rust-native parser types.

### 6.2 `libopforge::processing`

Use `processing` when you need editor-style routing that decides whether a line belongs to `opcore` or should fall through to assembler processing.

Key entrypoints:

- `editor_route_line`
- `editor_route_line_with_model`
- `editor_route_line_with_model_in_mode`
- `process_opcore_expression_request`
- `route_module_item_line`

This layer returns `LineProcessingTrace`, which is useful when you want to understand which processor claimed a line or expression request.

In `vm-runtime-only` builds, do not assume the CLI/runtime package fallback rules apply here. If your tool is calling the lower-level editor-routing helpers directly, build a facade-visible runtime model with `processing::HierarchyExecutionModel::from_registry(&registry::default_asm_registry())`, use `processing::register_checker_none()` or `processing::register_checker_from_fn(...)` for the tokenizer policy, and pass those values through the `*_with_model` entrypoints. Otherwise provide the default artifact file at `target/vm/opforge-vm-runtime.opasm` when `vm-runtime-opasm-artifact` is enabled.

If neither an explicit model nor the default artifact is available, the default helpers, including `route_module_item_line`, return a runtime-model-unavailable error rather than regenerating or bundling one implicitly.

### 6.3 `libopforge::asm::opasm`

Use `asm::opasm` when you want CPU-aware statement parsing or processing without running a full assembly session. Typical uses include statement-aware linting, per-line editor validation, instruction-shape inspection, and runtime-model experimentation.

`asm::opasm::ProcessorBuilder` is the main convenience entrypoint for CPU-aware processing. For `Vm` and `Lockstep` statement processing you must provide a `cpu_id`; the builder enforces that.

The `portable` submodule mirrors the pattern from `opcore`, exposing portable token and AST forms plus lockstep reporting.

## 7. Diagnostics and metadata

High-level assembly returns `AsmRunReport` on success and `AssemblerWorkflowError` on failure.
When assembly itself fails after request validation, the `AssemblerWorkflowError::Assemble` variant carries the underlying `AsmRunError` payload.

Detailed diagnostics, fixits, and source-map handling is documented in `documentation/libopforge-diagnostics-and-fixits-guide.md`.

Useful report accessors:

- `diagnostics()`
- `error_count()`
- `warning_count()`
- `source_lines()`
- `runtime_processing_traces()`
- `lockstep_report()`

The diagnostic model is richer than just line + message. A `Diagnostic` can carry:

- severity
- diagnostic code
- file and source text
- parser diagnostic attachment
- related spans
- notes
- help entries
- fixits

That makes the current report boundary suitable for IDEs, CI annotation, and
automated fixit flows.

Prepared sessions additionally expose:

- `SourceMap`, which maps expanded lines back to original source origins
- dependency file lists gathered during expansion/module loading

If your tool needs to present diagnostics against preprocessed sources but navigate back to original files, `SourceMap` is the first place to look. Host-facing consumption patterns are documented in `documentation/libopforge-diagnostics-and-fixits-guide.md`.

## 8. Registry and capability introspection

Use `libopforge::registry` when your tool needs to discover what the current build knows about CPUs or to validate user CPU selections before assembling.

Contributor-oriented CPU, family, and dialect work below the current facade is
documented in `documentation/libopforge-cpu-family-extension-guide.md` and
`documentation/libopforge-specification.md`.

Useful entrypoints:

- `default_asm_registry()`
- `resolve_target_cpu(...)`
- `AsmRegistry`
- `CpuType`
- `CpuFamily`

This is appropriate for:

- CPU pickers
- project-configuration validation
- diagnostics that want to report known CPU aliases/families
- capability/help UIs

Important limit: the preview facade exposes registry consumption and inspection
well, but it is not yet the polished high-level extension API for registering
brand new families from downstream code. For that work, go directly to the
workspace registry/family crates and treat it as advanced platform
development.

## 9. FFI and non-Rust hosts

`crates/opforge-ffi` mirrors the current Rust API shape rather than inventing a
separate orchestration model.

The FFI surface is split into:

- high-level `opforge_asm_*` entrypoints for assembly, session handles, prepared sessions, and reports
- lower-level `opforge_opcore_*` entrypoints for generic language tooling
- lower-level `opforge_opasm_*` entrypoints for statement tooling
- `opforge_registry_*`, `opforge_processing_*`, and `opforge_lockstep_*` groups for introspection

For non-Rust consumers, use the high-level `opforge_asm_*_with_request(...)` and session-oriented APIs built around `opforge_asm_request`.

Concrete non-Rust host recipes are in `documentation/libopforge-embedding-cookbook.md`.

If you are shipping the shared library for a host integration, build it with the unwind-safe FFI profile:

- `make build-ffi-release`, or
- `cargo build -p ffi --profile release-ffi --locked --lib`

Do not distribute the FFI library produced indirectly by a workspace-wide `cargo build --release`; the workspace release profile is `panic = "abort"`, while the report-returning FFI entrypoints are designed to return structured internal-error reports across panic boundaries. The plain convenience constructor `opforge_asm_session_create_with_request(...)` is intentionally a nullable-handle path and collapses internal panics to `NULL`.

The FFI implementation is a useful reference even for Rust developers because
it shows how to map the owned/session model into long-lived handles and
callback-driven I/O without bypassing the documented library path.

## 10. Existing tool integrations in this repo

These are the best reference points when you want to see how the library is used in practice:

- `documentation/libopforge-developer-guide-examples/libopforge_borrowed.rs`: minimal borrowed builder flow using `Assembler` plus memory-backed host adapters
- `documentation/libopforge-developer-guide-examples/libopforge_in_memory.rs`: minimal in-memory embedding example using `AssemblerSession`, `MemorySourceProvider`, and `MemoryOutputSink`
- `documentation/libopforge-developer-guide-examples/libopforge_filesystem.rs`: minimal filesystem-backed example using the same owned/session surface
- `documentation/libopforge-developer-guide-examples/libopforge_prepared.rs`: prepared-session example that inspects current metadata before final emission
- `documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs`: lockstep execution example that consumes `LockstepReport` matches through the facade
- `documentation/libopforge-developer-guide-examples/libopforge_workflow_error.rs`: current workflow-error inspection without reaching into internal crates
- `documentation/libopforge-developer-guide-examples/libopforge_opcore.rs`: expression parsing and module-item handling through `libopforge::opcore`
- `documentation/libopforge-developer-guide-examples/libopforge_registry.rs`: CPU capability discovery and target resolution via `libopforge::registry`
- `documentation/libopforge-developer-guide-examples/libopforge_formatter.rs`: source formatting through `libopforge::formatter`
- `documentation/libopforge-developer-guide-examples/libopforge_opasm.rs`: CPU-aware statement processing through `libopforge::asm::opasm`
- `crates/opforge-cli-core/src/run.rs`: useful for seeing how CLI options map onto the current assembler config model, even though it still goes through the internal `api` crate
- `crates/opforge-ffi/src/lib.rs`: shows how the session/report model is exposed to C and C++
- `crates/opforge-engine/src/processing.rs`: useful if you are implementing tooling near the editor-routing boundary

One nuance worth calling out: some internal workspace crates still depend on the internal `api` crate or lower-level crates directly. For external consumers, the guide examples under `documentation/libopforge-developer-guide-examples/` are the best model because they use the intended public `libopforge` facade directly.

## 11. Developing platform-native assemblers

This chapter is for contributors who want to make an 8-bit machine execute assembler behavior natively.

For a native C64-, Mega65-, or Atari-class runtime, the question is:

> What pieces of the current assembler pipeline must exist on the target machine so it can do the same work deterministically under tight memory and CPU limits?

### 11.1 Native goal

The native goal is not "port the whole Rust codebase."

The native goal is to provide 8-bit implementations of the runtime stages that matter on the hot path:

- load a runtime package and resolve the active pipeline
- tokenize one source line
- parse one source line into the expected envelope shape
- parse and evaluate expressions under the same contract rules
- resolve an instruction candidate and emit its bytes
- report deterministic diagnostics through a stable native ABI

If those pieces behave the same way as the current reference path, the native runtime can stand in for the Rust-hosted VM path.

### 11.2 Treat Rust as the executable specification

When doing native runtime work, the Rust implementation is most useful as an executable specification of:

- opcode meanings
- contract/version checks
- budget enforcement
- owner precedence (`dialect -> cpu -> family`)
- expected input/output shapes
- deterministic failure behavior

The most important reference documents are:

- `documentation/opforge-assembler-vm-path-guide-v0_1.md`
- `documentation/vm-boundary-protocol-v1.md`
- `documentation/vm-ultimate64-abi-contract-v1.md`

The most important reference code is:

- `crates/opforge-package/src/package.rs`
  opcode tables and contract descriptors
- `crates/opforge-vm/src/runtime_model_core.rs`
  runtime-model loading, lookup, and budget/compatibility enforcement
- `crates/opforge-vm/src/execution_model/parser_vm.rs`
  parser VM execution semantics
- `crates/opforge-vm/src/runtime_expr_parser.rs`
  authoritative `EXVM v2` parser execution semantics for covered expression grammar
- `crates/opforge-vm/src/vm_opcore.rs`
  opcore bridge points where assembler/runtime code crosses into `EXVM` and `EXPR`
- `crates/opforge-vm/src/bytecode.rs`
  encode-bytecode execution semantics
- `crates/opforge-core/src/expr_vm.rs`
  portable expression evaluator VM semantics, including `EXPR v2` structural values and explicit scalar-boundary enforcement

The target-native implementation should aim to reproduce those semantics, not the Rust source structure.

### 11.3 Native components you actually need

For an 8-bit-native runtime, the practical component list is:

- package loader
  parse the package/container format, deep-copy the runtime data you need, and reject unsupported versions deterministically
- pipeline selector
  resolve the active `dialect -> cpu -> family` ownership chain for the current request
- tokenizer VM executor
  run `TKVM` programs against one line buffer with the same budgets and diagnostic behavior
- parser VM executor
  run `PRVM` envelopes over the produced token stream and return the expected line shape
- expression parser VM executor
  run covered `EXVM v2` programs for bounded expression token ranges and report deterministic unsupported diagnostics for out-of-scope value nodes
- expression evaluator VM support
  run `EXPR v2` programs with typed values and explicit scalar-boundary enforcement for scalar-only callers
- encode bytecode executor
  run the minimal bytecode emitter that produces final instruction bytes from resolved operand byte slices
- native ABI surface
  expose stable entrypoints for init/load/set-pipeline/tokenize/parse/encode/error retrieval
- deterministic scratch-memory strategy
  decide where token buffers, parser state, expression stacks, temporary operand bytes, and last-error storage live

That is the native runtime core. Everything else is supporting integration work.

### 11.4 Know which VM lives where

The current runtime is not one VM. It is several small runtimes with different jobs.

For native bring-up, this split matters:

- Tokenizer VM (`TKVM`)
  opcode definitions: `crates/opforge-package/src/package.rs`
  reference executor: `crates/opforge-vm/src/runtime_model_core.rs`
- Parser VM (`PRVM`)
  opcode definitions: `crates/opforge-package/src/package.rs`
  reference executor: `crates/opforge-vm/src/execution_model/parser_vm.rs`
- Expression parser VM (`EXVM`)
  opcode definitions: `crates/opforge-package/src/package.rs`
  reference executor: `crates/opforge-vm/src/runtime_expr_parser.rs`
- Expression evaluator VM (`EXPR`)
  opcode definitions and reference executor: `crates/opforge-core/src/expr_vm.rs`
  active assembler path: `EXPR v2` with structural values plus explicit `RequireScalar` boundaries for scalar-only callers
- Encode bytecode VM
  reference executor: `crates/opforge-vm/src/bytecode.rs`
- Native 6502 harness surface
  reference ABI constants: `crates/opforge-vm/src/native6502_abi.rs`
  reference harness behavior: `crates/opforge-vm/src/native6502.rs`

Treat these as separate bring-up targets. On an 8-bit machine it is usually better to get one of them fully correct than to partially implement all of them at once.

### 11.5 The current native reference surface

The current reference for "how should a native runtime be called?" is the 6502-native harness.

Start with:

- `crates/opforge-vm/src/native6502_abi.rs`
- `crates/opforge-vm/src/native6502.rs`
- `documentation/vm-ultimate64-abi-contract-v1.md`

Those define the current stable envelope concepts:

- ABI magic and version
- fixed control-block layout
- entrypoint ordinals
- request/response expectations
- package load lifetime rules
- deterministic diagnostic/error namespaces

The key point is that a native runtime is expected to look like a compact service surface, not like a whole alternate build system.

### 11.6 What the host still does today

The current architecture still has Rust on the outside:

- preprocessing and include expansion
- module-graph loading
- macro expansion
- pass 1 / pass 2 orchestration
- image/list/map/hex/bin/label output emission

One narrow expression carveout also remains host-owned in the current assembler
boundary: repetition-side-table member/index forms such as
`repeatLabel[index].field` still reduce through the host because they depend on
assembler-owned iteration-scope side tables rather than generic `EXPR v2`
structural values.

Outside that carveout, covered authoritative-family expression parse/eval
failures stay on the VM/contract surface during pass 2 rather than silently
retrying host AST evaluation. Host scalar evaluation on those families is now
non-default: it occurs only through the explicit `FORCE_HOST` override controls
or through the host's provisional pass-1 unresolved-symbol placeholder rules
when a caller still needs a temporary numeric value before symbol finalization.

The active authoritative expression-family set now includes
`motorola68000` alongside `mos6502` and `intel8080`. That family claim covers
the audited `m68000`, `m68010`, `m68020`, `m68030`, `m68040`, and full
`m68080` expression-bearing paths under their existing legality checks,
including `m68080` integer, AMMX, Apollo-gated, and `fpu 68080`
expression-bearing callers.

That host work is coordinated mainly in:

- `crates/opforge-engine/src/lib.rs`
- `crates/opforge-engine/src/source_graph.rs`
- `crates/opforge-asm/src/engine.rs`
- `crates/opforge-asm/src/line.rs`

For native-runtime contributors, these files matter because they define:

- when the native runtime is invoked
- what inputs it receives
- what outputs it must return
- which work is still intentionally outside the native boundary

So yes, Rust is still part of the current end-to-end architecture. But for native-runtime work, it should be read as the outer coordinator and behavioral reference, not as the implementation focus.

### 11.7 Suggested bring-up order for an 8-bit implementation

The safest order for native bring-up is usually:

1. implement the control block / entrypoint ABI
2. implement package loading and pipeline selection
3. implement tokenizer VM execution
4. implement parser VM execution
5. implement expression parser VM execution for the covered `EXVM v2` surface
6. implement expression evaluation with `EXPR v2`, including explicit scalar-boundary checks for scalar-only callers
7. implement encode-bytecode execution
8. connect the full `load_package -> set_pipeline -> tokenize -> parse -> encode -> last_error` flow

That order keeps the surface testable at each step and mirrors the current native smoke-flow expectations documented in `documentation/vm-ultimate64-abi-contract-v1.md`.

### 11.8 Reading order for native implementation work

If the real goal is "make an 8-bit machine do what the Rust assembler path is doing," read in this order:

1. `documentation/opforge-assembler-vm-path-guide-v0_1.md`
2. `documentation/vm-boundary-protocol-v1.md`
3. `documentation/vm-ultimate64-abi-contract-v1.md`
4. `crates/opforge-vm/src/native6502_abi.rs`
5. `crates/opforge-vm/src/native6502.rs`
6. `crates/opforge-package/src/package.rs`
7. `crates/opforge-vm/src/runtime_model_core.rs`
8. `crates/opforge-vm/src/execution_model/parser_vm.rs`
9. `crates/opforge-vm/src/runtime_expr_parser.rs`
10. `crates/opforge-vm/src/vm_opcore.rs`
11. `crates/opforge-core/src/expr_vm.rs`
12. `crates/opforge-asm/src/line.rs`
13. `crates/opforge-engine/src/lib.rs`

That sequence keeps the focus on native runtime semantics first, then shows how the current host wraps those semantics into the larger assembler workflow.

## 12. Companion documents

The companion documents for the current branch are:

- `README.md`
- `documentation/opforge-assembler-vm-path-guide-v0_1.md`
- `documentation/libopforge-cpu-family-extension-guide.md`
- `documentation/libopforge-diagnostics-and-fixits-guide.md`
- `documentation/libopforge-embedding-cookbook.md`
- `documentation/libopforge-execution-modes-and-lockstep-guide.md`
- `documentation/libopforge-specification.md`
- `documentation/vm-boundary-protocol-v1.md`

The main topics covered by those documents are:

- `Assembler VM Path Guide`: assembler file-to-artifact flow, Rust/VM handoff points, current `opcore` VM coverage, and per-VM opcode references
- `Embedding Cookbook`: borrowed, owned, in-memory, prepared-session, and FFI-oriented embedding recipes
- `Execution Modes and Lockstep Guide`: `Vm`, `Rust`, and `Lockstep` mode selection and continuation-head behavior
- `Diagnostics and Fixits Guide`: diagnostics, fixits, source maps, and report consumption
- `CPU/Family Extension Guide`: contributor-facing CPU, family, and dialect extension work below the preview facade
- `libopforge-specification.md`: architecture boundaries and lower-level ownership rules
