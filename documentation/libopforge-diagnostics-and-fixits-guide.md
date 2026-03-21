# libopforge Diagnostics and Fixits Guide

This guide describes diagnostics, fixits, and related metadata in the current
published `libopforge` preview surface.

The API names and report types described here are usable today, but they are
still part of a pre-1.0 surface and may change across future `0.x` releases.

## 1. Scope

Topics covered here:

- surfacing assembler diagnostics in an IDE, editor, CI system, or service response
- deciding between `check()` and `assemble()` for validation-oriented workflows
- mapping structured fixits into a host UI or automated remediation flow
- navigating diagnostics back to original source locations after preprocessing or module expansion

## 2. Result-shape quick chooser

| Host concern | Start with | Why |
|---|---|---|
| validation without output-side effects | `check()` | normalizes output options so the run stays diagnostics-first |
| full assembly plus diagnostics | `assemble()` | preserves normal output behavior and still returns `AsmRunReport` on success |
| request-level or orchestration failure | `AssemblerWorkflowError` | captures failures before or around a successful `AsmRunReport` boundary |
| per-diagnostic UI and fixits | `AsmRunReport::diagnostics()` and `Diagnostic` | preserves structured codes, spans, notes, help, and fixits |
| remapping diagnostics to original source files | prepared-session `SourceMap` | bridges expanded/preprocessed positions back to original sources |

## 3. Current failure contract

High-level assembly returns `AsmRunReport` on success and `AssemblerWorkflowError` on failure.
When assembly itself fails after request validation, the `AssemblerWorkflowError::Assemble` variant carries the underlying `AsmRunError` payload.

Treat that split as deliberate:

- use `AssemblerWorkflowError` for request, orchestration, and top-level workflow handling
- use `AsmRunReport` and `Diagnostic` for the structured diagnostic payload produced by a completed run

Reference example: `documentation/libopforge-developer-guide-examples/libopforge_workflow_error.rs`.

## 4. Accessors that hosts usually need first

Common report accessors:

- `diagnostics()`
- `error_count()`
- `warning_count()`
- `source_lines()`
- `runtime_processing_traces()`
- `lockstep_report()` when parity workflows are in scope

Start with `diagnostics()` and the aggregate counts for ordinary host reporting. Pull in `runtime_processing_traces()` or `lockstep_report()` only when the host genuinely needs processor-routing or parity details.

## 5. What a `Diagnostic` can carry

The current diagnostic model is richer than line plus message. A `Diagnostic`
can carry:

- severity
- diagnostic code
- file and source text
- parser diagnostic attachment
- related spans
- notes
- help entries
- fixits

Preserve that structure as long as possible in the host. Do not flatten everything to plain text early if the UI or CI system can carry richer data.

## 6. Fixit handling

Use fixits when the host can present a precise corrective action or staged edit.

Recommended pattern:

1. surface the main diagnostic text, code, and span first
2. attach notes and help entries as secondary context
3. offer fixits as explicit user actions or structured edits rather than silently rewriting source

That keeps the host behavior explainable and avoids discarding structured remediation data.

## 7. Preprocessed-source navigation

Prepared sessions expose `SourceMap` plus dependency-file metadata gathered during expansion and module loading.

Use `SourceMap` when the host needs to:

- display diagnostics against original files instead of expanded source
- reconcile preprocessed lines with editor buffers
- explain how a generated or included line maps back to the source tree

If the host needs dependency awareness in the same flow, pair `SourceMap` with `dependency_files()` from the prepared boundary.

## 8. Pairing diagnostics with lower-level tooling

Use lower-level APIs when diagnostics must be incremental or editor-oriented rather than full-build oriented.

- `libopforge::opcore` for expression and module-item validation
- `libopforge::processing` for editor-style routing and processor-neutral failures
- `libopforge::asm::opasm` for CPU-aware statement validation without a full assembly session

Use those paths to improve interactivity, but keep the high-level `Diagnostic` contract as the normal full-run reporting shape.

## 9. Reference points in this repo

- `documentation/libopforge-developer-guide-examples/libopforge_workflow_error.rs`
- `documentation/libopforge-developer-guide-examples/libopforge_opcore.rs`
- `documentation/libopforge-developer-guide-examples/libopforge_opasm.rs`
- `documentation/libopforge-execution-modes-and-lockstep-guide.md` for parity-related report handling

The broader `libopforge` API boundary is described in `documentation/libopforge-developer-guide.md`.
