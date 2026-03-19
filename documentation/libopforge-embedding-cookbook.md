# libopforge Embedding Cookbook

This cookbook is the task-first companion to the main `libopforge` developer guide. Use it when you already know you are embedding the library into a host and want the shortest path to the right API shape.

## 1. Quick chooser

| If your host needs... | Start with | Why | Reference example |
|---|---|---|---|
| a short-lived Rust build or validation command | `libopforge::asm::Assembler::builder(...)` | borrowed builder flow with minimal host-owned state | `documentation/libopforge-developer-guide-examples/libopforge_borrowed.rs` |
| a long-lived service, worker, GUI, or callback-driven integration | `libopforge::asm::AssemblerSession::builder(...)` | owned/session shape keeps host state stable across requests | `documentation/libopforge-developer-guide-examples/libopforge_filesystem.rs` |
| in-memory sources or outputs | `libopforge::io::MemorySourceProvider` and `MemoryOutputSink` | avoids filesystem assumptions and fits editors, tests, and sandboxed hosts | `documentation/libopforge-developer-guide-examples/libopforge_in_memory.rs` |
| dependency inspection before final emission | `prepare()` or `AssemblerSession::prepare()` | exposes CPU choice, source map, and dependency files before output emission | `documentation/libopforge-developer-guide-examples/libopforge_prepared.rs` |
| a C or C++ host boundary | `crates/opforge-ffi` session and request APIs | mirrors the same owned/session model without inventing a second orchestration layer | `crates/opforge-ffi/src/lib.rs` |

## 2. Recipe: short-lived Rust build command

Use this when your host already owns filesystem paths and only needs one request at a time.

1. Start with `Assembler::builder(root_path)`.
2. Set only the request-specific knobs your command actually owns, such as `output_format(...)` or `cpu_override(...)`.
3. Call `check()` for validation-only commands or `assemble()` when you want emitted artifacts.

Use `check()` for background validation because it suppresses normal output-side effects. Use `assemble()` for real builds.

Reference example: `documentation/libopforge-developer-guide-examples/libopforge_borrowed.rs`.

## 3. Recipe: long-lived or callback-driven host

Use this when your host has a request pipeline, worker pool, GUI session, or other owned state that should outlive a single borrowed call.

1. Start with `AssemblerSession::builder(root_path)`.
2. Attach host-owned source and output adapters, explicit `output_base`, and any reusable config.
3. Reuse the session entry shape across validation and assembly requests.

This is the default recommendation for new integrations because it adapts cleanly to services, FFI handles, and non-filesystem hosts.

Reference examples:

- `documentation/libopforge-developer-guide-examples/libopforge_filesystem.rs`
- `documentation/libopforge-developer-guide-examples/libopforge_in_memory.rs`

## 4. Recipe: in-memory editor or test host

Use this when your host does not want temporary files for source input or emitted outputs.

1. Build a `MemorySourceProvider` with virtual file paths.
2. Capture outputs in `MemoryOutputSink`.
3. Use `bytes()` for arbitrary artifacts and `text()` only for text outputs.

This is the right fit for editors, browser-like hosts, tests, and worker-based integrations that keep source state outside the filesystem.

Reference example: `documentation/libopforge-developer-guide-examples/libopforge_in_memory.rs`.

## 5. Recipe: build preview before emission

Use a prepared flow when your host needs metadata before deciding whether to emit outputs.

1. Call `prepare()` or `AssemblerSession::prepare()`.
2. Inspect `root_module_id()`, `cpu_name()`, `source_map()`, and `dependency_files()`.
3. Decide whether to continue with `assemble()` or `check()` on the prepared value.

This is useful for IDE previews, dependency-graph reconciliation, and build systems that separate expansion from output emission.

Reference example: `documentation/libopforge-developer-guide-examples/libopforge_prepared.rs`.

## 6. Recipe: background validation in an existing tool

Use this when you want diagnostics without generating listings, hex, labels, or other outputs.

1. Map your source abstraction onto `SourceProvider`.
2. Map your artifact abstraction onto `OutputSink` only if your host also performs real assembly.
3. Call `check()` for validation commands.
4. Surface `Diagnostic` values directly instead of flattening them early.

If your tool also exposes line-level editing features, pair this with `libopforge::opcore`, `libopforge::processing`, or `libopforge::asm::opasm` rather than reparsing every edit as a full build.

## 7. Recipe: non-Rust host through FFI

Use this when the host cannot call the Rust facade directly.

1. Build the shared library with `make build-ffi-release` or `cargo build -p ffi --profile release-ffi --locked --lib`.
2. Use the `opforge_asm_*_with_request(...)` and session-oriented APIs around `opforge_asm_request`.
3. Treat the FFI boundary as a thin wrapper over the same session/report model documented for Rust hosts.

Do not ship the shared library from a workspace-wide `cargo build --release`; that build path uses the workspace release profile instead of the unwind-safe `release-ffi` profile required by the report-returning FFI entrypoints.

Reference implementation: `crates/opforge-ffi/src/lib.rs`.

## 8. Common rules that hold across recipes

- Prefer `libopforge` over lower-level workspace crates unless you are intentionally extending opForge internals.
- Start with `AssemblerSession` unless borrowed lifetimes are clearly the better fit for the host.
- Set `output_base` explicitly when artifact names must not be derived from `root_path`.
- Use memory-backed providers and sinks for tests, editors, overlays, and sandboxed environments.
- Use prepared flows when your host needs dependency or source-map metadata before emission.

For execution-mode selection, lockstep parity workflows, and continuation-head choices, continue with `documentation/libopforge-execution-modes-and-lockstep-guide.md`.