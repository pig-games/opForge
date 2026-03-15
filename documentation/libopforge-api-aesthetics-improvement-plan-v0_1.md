# libopforge API Aesthetics Improvement Plan

**Version:** 0.1-draft  
**Date:** March 15, 2026  
**Status:** active; refreshed against the squashed branch after the final review-fix round and legacy FFI compatibility removal

Execution breakdown:
- `documentation/libopforge-api-aesthetics-implementation-plan-v0_1.md`

## 1. Purpose

The current `libopforge` API is structurally strong but only moderately polished
as a downstream developer experience.

The public surface now has:

- a clear module-first root facade,
- a real embedding story,
- usable owned/session APIs,
- a much stronger tool boundary than `main`.

What it does **not** yet have is a fully relaxed, elegant, tactile feel for the
common path.

This plan is about aesthetic quality, not functional rescue.

It aims to improve:

- naming clarity,
- builder ergonomics,
- surface consistency,
- discoverability of the intended path,
- containment of advanced/transitional APIs.

It must do so **without** undoing the current architectural layering or
reverting to a flatter, less explicit design.

Working assumption for this revision:

- compatibility of the current facade naming is **not** a binding constraint,
  because the different public API surfaces are only consumed by opForge itself
  at this stage

## 2. Current Aesthetic Assessment

Overall score: **7/10**

Latest-state review note:

- README and the primary specification examples now use the live borrowed
  `Assembler::builder(&Path)` call shape
- the public embedding examples and much of the developer guide still lead
  with `OwnedAssemblerConfig` literals rather than builders
- the legacy high-level FFI compatibility bridge has now been removed, so FFI
  cleanup work is no longer distorting the API-aesthetics discussion

Those changes improve coherence, but they do not materially change the core
aesthetic diagnosis below.

### What already looks good

- The root facade in `src/lib.rs` is clean and memorable.
- The stable module map in `crates/opforge-lib/src/lib.rs` reflects the
  architecture honestly.
- `io::{SourceProvider, OutputSink}` is simple and host-friendly.
- `Assembler`, `AssemblerSession`, `PreparedAssembly`, and
  `PreparedAssemblySession` expose a sensible lifecycle.
- `AssemblerSessionBuilder` already reads like a reasonably complete
  downstream-facing builder.
- README and specification snippets now present the stable facade more
  intentionally than they did when this note was first drafted.
- The stable surface reads like a library, not like leaked binary plumbing.

### What currently feels awkward

- The configuration model is duplicated across borrowed and owned forms, then
  duplicated again across grouped config structs and request-shaped helper
  structs.
- The borrowed builder is much thinner than the owned/session builder, so the
  surface feels uneven.
- the user-facing name `input_base` describes an internal lineage more than the
  developer’s intent; in practice it is mostly an output-naming concept.
- `default_outputs` and `suppress_outputs` are accurate but aesthetically
  mechanical.
- `unstable` is too flat and reads like a second public API attic.
- the `normalized` naming in `opcore` and `asm::opasm` does not clearly signal
  that it is the portable-contract view.
- the public embedding examples and large parts of the developer guide still
  rely on large config literals, which undermines the stated builder-first
  ergonomic story.

## 3. Design Principles

This plan should preserve the current strengths.

### 3.1 Keep the module-first root facade

Do not collapse the surface back into flat re-exports.

Normal imports should continue to read like:

- `libopforge::asm::Assembler`
- `libopforge::io::MemorySourceProvider`
- `libopforge::diagnostics::Diagnostic`

### 3.2 Prefer final names over transitional aliases

Where a public name is clearly worse than the intended long-term name, prefer:

- direct renames,
- direct module moves,
- removal of transitional spellings,

instead of layering aliases or compatibility shims.

Compatibility-preserving aliases should only be used if they materially reduce
short-term implementation risk inside the workspace itself.

### 3.3 Improve the common path first

The best aesthetic win is not deep type surgery.

It is making the first successful use of the library feel shorter, clearer, and
more obviously intended.

### 3.4 Keep explicitness for advanced flows

The goal is not “magical convenience.”

The goal is:

- a short pleasant path for the common case,
- explicit grouped control for the serious case,
- contained escape hatches for advanced or transitional work.

## 4. Concrete Aesthetic Problems

### 4.1 The public surface now demonstrates two different ergonomic stories

README and the specification now point developers toward the borrowed builder
path, but the public embedding examples and developer guidance still mostly
teach the library through large config literals.

That split makes the project look more polished at the top level than it feels
once a developer opens the examples they are most likely to copy.

The examples in:

- `examples/libopforge_in_memory.rs`
- `examples/libopforge_filesystem.rs`
- `documentation/libopforge-developer-guide.md`

still teach the API primarily through large `OwnedAssemblerConfig` literals.

That is a correctness-preserving style, but not an aesthetically persuasive one.

### 4.2 Borrowed and owned builders feel like different products

The borrowed builder (`AssemblerBuilder`) has grown slightly and now carries
request-scoped `opasm_package_path`, but it still exposes a much smaller and
less expressive surface than `AssemblerSessionBuilder`.

This makes the borrowed path feel incidental and the owned path feel “real,”
even though the README explicitly states that ownership choice should change
ergonomics rather than capability.

### 4.3 Some public names leak internal history

Examples:

- `input_base`
- `default_outputs`
- `suppress_outputs`
- `normalized`

These names are not wrong. They just do not all read like names designed from
the perspective of a tool author encountering the library fresh.

### 4.4 `unstable` now looks more like a draining area than a namespace to refine

`unstable` currently aggregates three different kinds of things:

- functionality that already has a better stable home and is duplicated here,
- tool-facing exports that may deserve first-class stable modules,
- raw engine/request APIs that are useful internally but do not yet justify
  facade-level exposure.

That makes it feel less like an intentional advanced namespace and more like a
holding zone whose contents should either graduate or leave.

### 4.5 The config family is honest but visually heavy

The current public type set includes:

- `AssemblerConfig`
- `OwnedAssemblerConfig`
- `SourceOptions`
- `OwnedSourceOptions`
- `ExecutionOptions`
- `OwnedExecutionOptions`
- `OutputOptions`
- `OwnedOutputOptions`
- `PrepareOptions`
- `AssembleOptions`

This is understandable once learned, but it does not yet feel spare.

Recent parity work also added request-scoped `opasm_package_path` across the
execution config family. That change is functionally correct and desirable, but
it strengthens the case for making the ergonomic path lighter as the explicit
type matrix grows.

The goal should not be to erase this structure blindly; it should be to make
the visible ergonomic path lighter so that the structural richness sits behind
it rather than in front of it.

## 5. Recommended Improvement Program

### 5.0 Latest-state status snapshot

- Slice A is **not started**
- Slice B is **partially complete**:
  - README and the main specification example now use the correct borrowed
    builder call shape
  - crate-local tests already exercise both borrowed and owned builder flows
  - the public embedding examples and most developer-guide snippets still lead
    with owned config literals rather than builders
- Slice C is **partially improved but still far from complete**:
  - the borrowed builder now supports `input_base`, `source_provider`,
    `execution_mode`, `opasm_package_path`, `out_dir`, `output_format`,
    `label_output_format`, `header_title`, and `output_sink`
  - the broader parity gap with `AssemblerSessionBuilder` remains
- Slice D is **not started**
- Slice E is **not started**
- Slice F remains **deferred**, but its urgency has increased as the execution
  config family has grown again
- legacy FFI compatibility removal is **complete** and is no longer a planning
  constraint for aesthetic cleanup
- stable-surface compatibility is **not** a planning constraint for naming work
  on this branch

### 5.1 Slice A: adopt developer-facing names directly

Replace historically accurate but aesthetically awkward names with the intended
long-term public names.

Primary recommendation:

- rename `input_base` to `output_base`

Rationale:

- the README already has to explain that `input_base` is really about output
  naming behavior,
- “output base” is the phrase most embedders will infer naturally from the
  behavior,
- there is no external compatibility obligation forcing the project to keep the
  older term alive.

Secondary recommendations:

- rename `normalized` to `portable` in:
  - `libopforge::opcore`
  - `libopforge::asm::opasm`
- update docs and examples to use the final names directly rather than teaching
  both spellings
- keep existing “normalized” language in the deeper lockstep/specification docs
  where it describes comparison semantics rather than only the public module
  name
- describe this view as the “portable contract” view in public developer docs.

Recommended success conditions:

- common docs can avoid explaining `input_base` as a conceptual exception,
- the portable/normalized distinction becomes visually obvious,
- the stable API no longer carries history-driven vocabulary just to preserve
  temporary spellings.

### 5.2 Slice B: make the intended ergonomic path visible

This becomes the highest-value follow-on slice once the public names are final.

Work items:

- Switch the primary public examples to builder-first forms where practical.
- Keep one explicit config-literal example for advanced hosts, but do not make
  it the first thing developers see.
- Update `examples/libopforge_in_memory.rs` and
  `examples/libopforge_filesystem.rs` to use
  `AssemblerSession::builder(...)` as the primary example shape.
- Update the developer guide so its first in-memory and filesystem examples use
  the builder-oriented owned/session path rather than full
  `OwnedAssemblerConfig` literals.
- Add a short ownership-choice table to the developer guide:
  - borrowed `Assembler`
  - owned `AssemblerSession`
  - when to prefer each
- Keep one compact grouped-config example in the developer guide for hosts that
  need to mirror an existing config model exactly.
- Make the public docs show the common path in fewer moving parts.

Recommended success condition:

- a new developer can copy a builder-oriented example without first learning the
  full grouped config literal structure.

Concrete target shape:

```rust
let report = AssemblerSession::builder("/virtual/main.asm")
    .output_base("/virtual/main")
    .source_provider(source_provider)
    .output_sink(output_sink)
    .execution_mode(ExecutionMode::Vm)
    .prepare()?
    .assemble()?;
```

This slice should be executed after Slice A so the examples teach the final
surface rather than transitional names.

### 5.3 Slice C: bring builder parity to the borrowed path

The builder is part of the public aesthetic contract.

Right now:

- `AssemblerBuilder` feels thin and partial,
- `AssemblerSessionBuilder` feels much more complete.

Recommended direction:

- extend `AssemblerBuilder` toward capability parity with the stable borrowed
  config surface, matching the owned/session builder wherever lifetimes allow,
- keep ownership differences explicit, but do not make them read like two
  unrelated DSLs.

This should include borrowed-builder support for the same grouped concerns that
the owned/session builder already surfaces:

- defines
- include/module paths
- preprocessor depth
- CPU override
- loop limits
- go address
- bin specs
- fill byte
- labels/dependency output
- output-name overrides
- default output policy
- diagnostics toggles

Concrete missing borrowed-builder controls today include:

- `defines(...)`
- `include_paths(...)`
- `module_paths(...)`
- `pp_macro_depth(...)`
- `cpu_override(...)`
- `max_loop_iterations(...)`
- `go_addr(...)`
- `bin_specs(...)`
- `fill_byte(...)`
- `labels_file(...)`
- `dependency_output(...)`
- `outfile_override(...)`
- `list_name_override(...)`
- `hex_name_override(...)`
- `default_outputs(...)`
- `debug_conditionals(...)`
- `tab_size(...)`

Both builders currently also lack a friendlier `suppress_outputs`-style
convenience, but that belongs to Slice D more than Slice C.

Recommended success conditions:

- borrowed and owned builders differ mainly in value ownership, not in API
  grammar,
- the README statement that ownership choice changes ergonomics rather than
  capability becomes aesthetically true as well as semantically true.

### 5.4 Slice D: tame output-policy vocabulary

The current output control knobs are explicit but a little mechanical:

- `default_outputs`
- `suppress_outputs`

Short-term recommendation:

- rename the builder-level output controls toward intent-expressive names
- likely direction:
  - `suppress_outputs` -> `no_outputs`
  - reconsider whether `default_outputs` should remain a boolean or become a
    more explicit output-emission control

Medium-term recommendation:

- consider whether output emission should eventually be represented by a small
  stable enum rather than paired booleans.

Example conceptual direction:

```rust
enum ArtifactEmission {
    Default,
    Suppressed,
}
```

This should only be done if it simplifies both Rust and FFI stories cleanly.
It is not urgent enough to justify churn by itself.

Recommended success condition:

- common code reads like intent instead of policy wiring.

### 5.5 Slice E: drain `unstable` by promotion or removal

Do not spend design energy making `unstable` prettier if its long-term role is
to disappear.

Recommended direction:

- promote the parts that have a credible stable story and a natural module home
- remove facade re-exports that are duplicated by existing stable modules
- remove raw engine/request exports from the facade when their main consumers
  are internal workspace crates that can depend on lower-level crates directly

Initial classification from the current branch state:

Promote candidates:

- formatter surface:
  - `FormatterConfig`
  - `FormatterEngine`
  - `FormatMode`
  - formatter report/output/diagnostic types
- registry-facing report helpers if they are considered part of the supported
  host story:
  - `capabilities_report`
  - `capabilities_report_json`
  - `cpusupport_report`
  - `cpusupport_report_json`

Remove as redundant facade spillover:

- `build_default_asm_registry`
- `AsmRegistry`
- `AsmRegistryContext`
- `CapabilitySnapshot`
- `CpuCapabilityView`
- `CpuResolutionError`
- `resolve_target_cpu`
- `editor_route_line`
- `editor_route_line_with_model`
- `editor_route_line_with_model_in_mode`
- `process_opcore_expression_request`
- `process_opcore_expression_request_with_mode`
- `LineProcessingTrace`
- `OpcoreRequestKind`
- `ProcessingOutcome`
- `ProcessingRequestKind`
- `ProcessingReturn`

Remove from the facade unless a stronger public use case is documented:

- `build_default_runtime_package_bytes`
- `effective_include_paths_for_root`
- `expand_source_file`
- `expand_source_file_with_dependencies`
- `expand_source_file_with_dependencies_with_provider`
- `parse_cpu_directive_name`
- `prepare_assembly_session`
- `remap_diagnostics_with_source_map`
- `resolve_cpu_for_line`
- `resolve_formatter_module_paths`
- `resolve_output_plan`
- `root_module_id_from_lines`
- `run_assembly`
- `run_prepared_assembly`
- `scan_cpu_transitions`
- `warnings_as_errors`
- `AssemblerSessionConfig`
- `AssemblyExecutionRequest`
- `AssemblyPreparationRequest`
- `FormatterPathResolutionRequest`
- `OutputPlanningRequest`
- `PreparedAssemblyExecutionRequest`
- `ResolvedOutputPlan`
- `run_lsp_stdio`
- `run_lsp_stdio_with_registry`
- `run_lsp_stdio_default`

Recommended success conditions:

- `libopforge` no longer presents `unstable` as a broad second facade
- every remaining promoted item has a clear stable module home
- internal tools that need raw engine plumbing depend on lower-level crates
  rather than the facade attic

### 5.6 Slice F: reduce visual weight without erasing grouped config types

Do **not** rush to delete the grouped config model.

It has real architectural value.

Instead, separate:

- the ergonomic entry path,
- the explicit full-control path.

Recommended direction:

- keep grouped public config types for explicit hosts,
- make the builder and examples the default public teaching surface,
- revisit deeper consolidation only if the aesthetic problems remain after the
  builder/doc improvements land.

Possible later exploration:

- internal unification of borrowed/owned config construction
- shared implementation helpers behind the public grouped types
- fewer public request-shaped helper types if they are not carrying their own
  ergonomic weight

This is an intentionally deferred slice.

## 6. Changes To Avoid Right Now

The following may be aesthetically tempting but are poor near-term trades:

- renaming `AsmRunReport` / `AsmRunError` purely for style,
- removing `Owned*` types before the owned/session story is materially simpler,
- replacing the grouped config model with a single giant ergonomic struct,
- flattening advanced functionality into the root namespace,
- broad rename churn without a clearly better destination name or module home.

These would create more churn than polish at this stage.

## 7. Recommended Order

Implement in this order:

1. Slice A: direct naming cleanup (`output_base`, `portable`)
2. Slice B: docs/examples and ownership-choice presentation
3. Slice C: borrowed-builder parity
4. Slice E: promote-or-remove pass for `unstable`
5. Slice D: output-policy convenience vocabulary
6. Slice F only if the surface still feels too heavy after the earlier slices

Why this order:

- it lands the final names before the public examples are rewritten around
  them,
- it still delivers visible polish early,
- it avoids spending effort on transitional compatibility shims,
- it tests whether the main aesthetic pain is teaching/ergonomics rather than
  deeper type architecture.

## 8. Success Criteria

This plan succeeds if, after the early slices:

- the first example a developer sees is builder-first and clearly intentional,
- the borrowed and owned builders look like members of the same API family,
- the most common naming friction (`input_base`) has a friendlier path,
- `unstable` is either gone or reduced to a very small residual set on a clear
  path to disappearance,
- the stable surface still looks architecturally explicit rather than magically
  flattened.

## 9. Short Version

The API does **not** need a redesign-from-scratch.

It needs:

- better teaching of the intended path,
- direct naming cleanup,
- builder parity,
- promotion or removal of the overflow surface.

The architecture is already the elegant part.
The next step is making the ergonomics feel as intentional as the layering.
