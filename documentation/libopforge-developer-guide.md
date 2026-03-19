# libopforge Developer Guide

This guide is for developers who want to embed opForge as a library, build new tools on top of it, or integrate it into an existing build/editor/runtime environment.

It documents the stable libopforge host surface in this branch/worktree as of `v0.9.5`.

## 1. Start here

If you are building:

| Use case | Start with | Why |
|---|---|---|
| Rust CLI, build tool, GUI, server | `libopforge::asm` | This is the supported high-level embedding surface |
| In-memory editor, browser-like host, tests | `libopforge::asm` + `libopforge::io` | Swap filesystem I/O for memory-backed providers/sinks |
| Background validation or repeated builds | `AssemblerSession::builder(...)`, `prepare()`, `check()` | These separate ownership, preparation, and execution cleanly while keeping the host entry path builder-first |
| Syntax-aware editor features | `libopforge::opcore`, `libopforge::processing`, `libopforge::asm::opasm` | These expose tokenization, expression parsing, line routing, and statement processing |
| CPU discovery or support UI | `libopforge::registry` | Introspect builtin CPUs/families/dialects without touching internals |
| Formatter hosts | `libopforge::formatter` | Access the stable formatter surface without depending on lower-level crates |
| C or C++ host integration | `crates/opforge-ffi` | Thin ABI layer over the same library/session model |
| New CPU/family/dialect implementations | direct workspace crates (`registry`, `families`, `vm`) | This is advanced extension work, not the normal stable embedding path |

Recommendation: for external Rust consumers, depend on `libopforge` and stay inside its module tree unless you are intentionally extending opForge itself.

## 2. Workspace map

The workspace is deliberately layered.

- `crates/opforge-lib` (package name `libopforge`) is the published facade crate. It defines the supported module layout and is the crate downstream Rust code should import.
- the facade remains curated and host-facing. It defines the stable module groups, config types, session types, and high-level helpers directly from the crate-owned layout.
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
4. `libopforge` presents the supported host boundary over those pieces.

## 3. Stable API boundary

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

- The normal supported Rust embedding path is `libopforge::asm::Assembler` or `libopforge::asm::AssemblerSession`.
- The `libopforge` package in `crates/opforge-lib` is the public facade. External consumers should target that crate rather than the lower-level workspace crates directly.
- The registry module in the stable facade is primarily for lookup and introspection. Full custom family/CPU registration is still an advanced lower-level workflow in the workspace crates.

### 3.1 Concern inventories

The stable facade is organized by concern ownership. Use the module-first API and treat each module below as the canonical home for that concern boundary.

| Module | Owns | Does not own |
|---|---|---|
| `libopforge::asm` | High-level assembly embedding, grouped config and session lifecycle, assembler workflow errors, and output-oriented assembly helpers | Generic language parsing, processor-neutral routing, or CPU discovery as primary concerns |
| `libopforge::asm::opasm` | CPU-aware statement tokenization, parsing, processing, and portable statement forms without full assembly | Full assembly orchestration, artifact emission, or registry discovery |
| `libopforge::opcore` | Generic non-assembler language services: tokenization, expressions, module items, macros, preprocess, and `CoreError` | Assembler statement encoding, listings, or artifact output |
| `libopforge::processing` | Processor-neutral routing, processing traces, neutral processor failures, and editor-style line dispatch | High-level assembler workflow packaging or generic language ownership |
| `libopforge::diagnostics` | Stable assembler diagnostics, run reports, and assembler diagnostic taxonomy | Source loading, registry discovery, or execution-mode policy |
| `libopforge::io` | Stable filesystem and memory-backed source and output adapters | Diagnostics taxonomy, CPU selection, or assembly semantics |
| `libopforge::registry` | CPU, family, and capability lookup plus builtin registry introspection | Full custom extension authoring as a stable facade concern |
| `libopforge::lockstep` | Execution-head selection, lockstep checkpoints, and parity reporting | Statement parsing, assembly workflow, or registry discovery |
| `libopforge::formatter` | Stable formatter configuration, formatter runs, and formatter reports | Reclassifying assembler or opcore diagnostics |

CLI or host presentation may specialize wording more than lower API layers do. The stable library boundary preserves structured categories, codes, and ownership boundaries even when higher-level tools choose different user-facing phrasing.

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

In the stable Rust API, when `output_base` is omitted, the public `assemble()` and `prepare()` paths derive it from `root_path` by removing the source extension. If your host wants artifact names that differ from that path-derived default, set `output_base` explicitly.

### 4.2 Execution mode defaults

The default execution mode in the stable config types is `ExecutionMode::Vm`.

Use:

- `ExecutionMode::Vm` for the normal current default path.
- `ExecutionMode::Rust` when you want the native Rust continuation head.
- `ExecutionMode::Lockstep { continuation_head: ... }` when validating Rust/VM parity and consuming a `LockstepReport`.

Reference example:

- `documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs`

### 4.3 `check()` versus `assemble()`

Use `check()` when you want validation without output-side effects.

In the stable API, `check()` explicitly normalizes the output configuration so it disables default outputs, labels, dependency output, binary specs, and output-file overrides before assembling. Internally that means forcing `no_outputs = true`, which makes `check()` the right background-validation call for editor tooling and CI-style validation.

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

### 5.1 Borrowed host integration

Use `Assembler::builder(...)` when your host already owns its providers and sinks and can keep them alive for the duration of the call.

The borrowed config structs are non-exhaustive. Treat the builder as the supported construction path for new code, and prefer `&Path` for single borrowed path-valued setters such as `out_dir(...)`, `labels_file(...)`, and `opasm_package_path(...)`.

```rust
use libopforge::asm::{Assembler, OutputFormat};

let report = Assembler::builder(std::path::Path::new("src/main.asm"))
    .output_format(OutputFormat::Text)
    .check()?;
```

This is a good fit for build tools, command wrappers, and short-lived requests.

### 5.2 Owned or non-borrowing host integration

Use `AssemblerSession::builder(...)` when your host needs owned state, long-lived sessions, or FFI-friendly ergonomics.

The owned grouped config types are also non-exhaustive. They remain useful as long-lived host state, but the builder/session entrypoints are the stable ergonomic path the facade teaches first.

The workspace's public examples use this model because it maps cleanly onto in-memory and callback-oriented hosts.

Reference examples:

- `documentation/libopforge-developer-guide-examples/libopforge_borrowed.rs`
- `documentation/libopforge-developer-guide-examples/libopforge_in_memory.rs`
- `documentation/libopforge-developer-guide-examples/libopforge_filesystem.rs`
- `documentation/libopforge-developer-guide-examples/libopforge_prepared.rs`
- `documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs`

### 5.3 In-memory integration

For editors, tests, and embedded hosts, use the concrete memory adapters re-exported from `libopforge::io`.

```rust
use libopforge::asm::{AssemblerSession, ExecutionMode};
use libopforge::io::{MemoryOutputSink, MemorySourceProvider};

let source_provider = MemorySourceProvider::new().with_file(
    "/virtual/main.asm",
    ".module main\n    .byte $00\n.endmodule\n",
);
let output_sink = MemoryOutputSink::new();

let report = AssemblerSession::builder("/virtual/main.asm")
    .output_base("/virtual/main")
    .source_provider(source_provider.clone())
    .output_sink(output_sink.clone())
    .execution_mode(ExecutionMode::Vm)
    .assemble()?;

assert_eq!(report.error_count(), 0);
```

Use `MemoryOutputSink::bytes()` for arbitrary captured artifacts. `MemoryOutputSink::text()` is for text outputs only and returns `Result<Option<String>, FromUtf8Error>` so binary artifacts do not panic the host.

`SourceProvider` and `OutputSink` are `Send + Sync`, so it is straightforward to share them across worker threads if your host architecture requires it.

### 5.4 Reusing prepared state

If your tool needs stable metadata before deciding whether to emit outputs, use a prepared session:

```rust
let prepared = AssemblerSession::with_config(root_path, config).prepare()?;
let root_module = prepared.root_module_id().to_string();
let cpu = prepared.cpu_name().to_string();
let dependencies = prepared.dependency_files().to_vec();
let report = prepared.assemble()?;
```

This is a good fit for IDE build previews, incremental build orchestration, or any tool that wants to record dependency edges separately from the final artifact run.

## 6. Tooling-oriented lower-level APIs

Not every tool wants full assembly. The stable facade also exposes lower-level services for syntax-aware tooling.

### 6.1 `libopforge::opcore`

Use `opcore` when you need generic language services that are not tied to full assembly:

- `tokenize_line`
- `parse_expression`
- `parse_expression_tokens`
- `process_module_item`

This is a good fit for:

- syntax highlighting and token pipelines
- expression-aware UIs
- quick validation of `.use`, `.module`, and related module-item forms

The `portable` submodule exposes portable token and AST structures derived from the VM portable contract. Use this when your tool wants a serialization-friendly or FFI-friendly view rather than Rust-native parser types.

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

Use `asm::opasm` when you want CPU-aware statement parsing or processing without running a full assembly session.

Typical uses:

- statement-aware linting
- per-line editor validation
- instruction-shape inspection
- parity or runtime-model experimentation

`asm::opasm::ProcessorBuilder` is the main convenience entrypoint for CPU-aware processing. For `Vm` and `Lockstep` statement processing you must provide a `cpu_id`; the builder enforces that.

The `portable` submodule mirrors the pattern from `opcore`, exposing portable token and AST forms plus lockstep reporting.

## 7. Diagnostics and metadata

High-level assembly returns `AsmRunReport` on success and `AssemblerWorkflowError` on failure.
When assembly itself fails after request validation, the `AssemblerWorkflowError::Assemble` variant carries the underlying `AsmRunError` payload.

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

That makes the stable report boundary suitable for IDEs, CI annotation, and automated fixit flows.

Prepared sessions additionally expose:

- `SourceMap`, which maps expanded lines back to original source origins
- dependency file lists gathered during expansion/module loading

If your tool needs to present diagnostics against preprocessed sources but navigate back to original files, `SourceMap` is the first place to look.

## 8. Registry and capability introspection

Use `libopforge::registry` when your tool needs to discover what the current build knows about CPUs or to validate user CPU selections before assembling.

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

Important limit: the stable facade exposes registry consumption and inspection well, but it is not yet the polished high-level extension API for registering brand new families from downstream code. For that work, go directly to the workspace registry/family crates and treat it as advanced platform development.

## 9. FFI and non-Rust hosts

`crates/opforge-ffi` mirrors the stable Rust API shape rather than inventing a separate orchestration model.

The FFI surface is split into:

- high-level `opforge_asm_*` entrypoints for assembly, session handles, prepared sessions, and reports
- lower-level `opforge_opcore_*` entrypoints for generic language tooling
- lower-level `opforge_opasm_*` entrypoints for statement tooling
- `opforge_registry_*`, `opforge_processing_*`, and `opforge_lockstep_*` groups for introspection

For non-Rust consumers, use the high-level `opforge_asm_*_with_request(...)` and session-oriented APIs built around `opforge_asm_request`.

If you are shipping the shared library for a host integration, build it with the unwind-safe FFI profile:

- `make build-ffi-release`, or
- `cargo build -p ffi --profile release-ffi --locked --lib`

Do not distribute the FFI library produced indirectly by a workspace-wide `cargo build --release`; the workspace release profile is `panic = "abort"`, while the report-returning FFI entrypoints are designed to return structured internal-error reports across panic boundaries. The plain convenience constructor `opforge_asm_session_create_with_request(...)` is intentionally a nullable-handle path and collapses internal panics to `NULL`.

The FFI implementation is a useful reference even for Rust developers because it shows how to map the owned/session model into long-lived handles and callback-driven I/O without bypassing the stable library path.

## 10. Existing tool integrations in this repo

These are the best reference points when you want to see how the library is used in practice:

- `documentation/libopforge-developer-guide-examples/libopforge_borrowed.rs`: minimal borrowed builder flow using `Assembler` plus memory-backed host adapters
- `documentation/libopforge-developer-guide-examples/libopforge_in_memory.rs`: minimal in-memory embedding example using `AssemblerSession`, `MemorySourceProvider`, and `MemoryOutputSink`
- `documentation/libopforge-developer-guide-examples/libopforge_filesystem.rs`: minimal filesystem-backed example using the same owned/session surface
- `documentation/libopforge-developer-guide-examples/libopforge_prepared.rs`: prepared-session example that inspects stable metadata before final emission
- `documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs`: lockstep execution example that consumes `LockstepReport` matches through the facade
- `documentation/libopforge-developer-guide-examples/libopforge_workflow_error.rs`: stable workflow-error inspection without reaching into internal crates
- `documentation/libopforge-developer-guide-examples/libopforge_opcore.rs`: expression parsing and module-item handling through `libopforge::opcore`
- `documentation/libopforge-developer-guide-examples/libopforge_registry.rs`: CPU capability discovery and target resolution via `libopforge::registry`
- `documentation/libopforge-developer-guide-examples/libopforge_formatter.rs`: source formatting through `libopforge::formatter`
- `documentation/libopforge-developer-guide-examples/libopforge_opasm.rs`: CPU-aware statement processing through `libopforge::asm::opasm`
- `crates/opforge-cli-core/src/run.rs`: useful for seeing how CLI options map onto the current assembler config model, even though it still goes through the internal `api` crate
- `crates/opforge-ffi/src/lib.rs`: shows how the session/report model is exposed to C and C++
- `crates/opforge-engine/src/processing.rs`: useful if you are implementing tooling near the editor-routing boundary

One nuance worth calling out: some internal workspace crates still depend on the internal `api` crate or lower-level crates directly. For external consumers, the guide examples under `documentation/libopforge-developer-guide-examples/` are the best model because they use the intended public `libopforge` facade directly.

## 11. Guidance for new tools

If you are starting a new integration today:

- Prefer `libopforge` only. Add direct dependencies on `engine`, `asm`, `vm`, or `registry` only if you are intentionally participating in opForge internals.
- Start with `AssemblerSession` unless you know borrowed lifetimes are the best fit. The owned/session shape is easier to adapt to services, worker pools, and FFI.
- Use `check()` for background validation and save `assemble()` for artifact-producing actions.
- Use `Assembler::prepare()` or `AssemblerSession::prepare()` when you need a reusable pre-expanded session boundary.
- Use the free `prepare()` helper when you mainly want preparation metadata plus a reusable execution mode and do not need the broader prepared-output configuration surface.
- Use memory-backed providers and sinks for tests, editor overlays, or non-filesystem hosts.
- Use `opcore` and `processing` for language tooling instead of trying to reverse-engineer assembler execution paths.
- Use `asm::opasm` for per-statement CPU-aware tooling instead of spinning up a full assembly pipeline.
- If you need lower-level workspace crates, depend on them explicitly rather than expecting overflow exports from the facade.

## 12. Guidance for integrating into an existing tool

If you already have a tool and want to add opForge support, the easiest mapping is usually:

1. Map your source abstraction onto `SourceProvider`.
2. Map your artifact/output abstraction onto `OutputSink`.
3. Translate your config model into `OwnedAssemblerConfig`.
4. Use `check()` for validation commands and `assemble()` for real builds.
5. Surface `Diagnostic` data directly in your UI instead of flattening it early.
6. If your tool already has a dependency graph, use a prepared session and `dependency_files()` to reconcile opForge's view with your own.
7. If your tool exposes line-level editing features, add `opcore`/`processing` calls instead of reparsing everything as full builds.

That approach lets opForge adapt to an existing host architecture without forcing the host to adopt filesystem-only assumptions.

## 13. Documentation roadmap and decision boundaries

This guide remains the entrypoint for `libopforge`, but it should not become the catch-all reference for every host-facing task. Use the matrix below to decide where detail belongs as the companion documents land.

| If you are trying to... | Primary document | Decision boundary |
|---|---|---|
| embed `libopforge` into a CLI, IDE, service, FFI host, or test harness | this guide today; future `Embedding Cookbook` | keep orientation, stable facade entrypoints, and end-to-end host setup here; move task recipes and host-pattern walkthroughs into the cookbook |
| choose between Rust and VM execution, understand lockstep expectations, or reason about parity-sensitive runtime paths | this guide today; future `Execution Modes and Lockstep` guide | keep the high-level runtime model here; move detailed mode-selection rules, parity notes, and lockstep workflows into the runtime guide |
| extend CPU or family support below the stable facade | `documentation/libopforge-specification.md` today; future `CPU/Family Extension Guide` | keep public architecture boundaries in the specification; move contributor implementation steps and extension checklists into the extension guide |
| integrate diagnostics, fixits, and editor or CI feedback loops | this guide today; future `Diagnostics and Fixits` guide | keep the stable diagnostic contract here; move IDE, CI, and fixit-consumer recipes into the diagnostics guide |

Until those companion guides are published, keep detailed material in the closest matching section above and update this matrix whenever content moves.

The maintained companion documents for the current branch are:

- `README.md`
- `documentation/libopforge-specification.md`
- `documentation/vm-boundary-protocol-v1.md`
