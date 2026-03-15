# libopforge API Aesthetics Improvement Plan

**Version:** 0.1-draft  
**Date:** March 14, 2026  
**Status:** active; reviewed against latest branch state after issue 4/5/6 follow-on work

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

## 2. Current Aesthetic Assessment

Overall score: **7/10**

Latest-state review note:

- the primary specification example now uses the live borrowed
  `Assembler::builder(&Path)` call shape
- grouped high-level FFI parity is materially better than when this note was
  first drafted, including request-scoped execution package selection

Those changes improve coherence, but they do not materially change the core
aesthetic diagnosis below.

### What already looks good

- The root facade in `src/lib.rs` is clean and memorable.
- The stable module map in `crates/opforge-lib/src/lib.rs` reflects the
  architecture honestly.
- `io::{SourceProvider, OutputSink}` is simple and host-friendly.
- `Assembler`, `AssemblerSession`, `PreparedAssembly`, and
  `PreparedAssemblySession` expose a sensible lifecycle.
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
- the public examples still rely on large config literals, which undermines the
  stated builder-first ergonomic story.

## 3. Design Principles

This plan should preserve the current strengths.

### 3.1 Keep the module-first root facade

Do not collapse the surface back into flat re-exports.

Normal imports should continue to read like:

- `libopforge::asm::Assembler`
- `libopforge::io::MemorySourceProvider`
- `libopforge::diagnostics::Diagnostic`

### 3.2 Prefer additive polish over renaming churn

Where the current names are functional but inelegant, prefer:

- aliases,
- clearer builder methods,
- better examples,
- better grouping,

before considering hard renames.

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

### 4.1 The surface tells one ergonomic story and demonstrates another

The docs say the API is builder-oriented and grouped by concern, but the public
embedding examples and developer guidance still mostly teach the API through
large config literals.

The primary specification snippet has improved, but the examples in:

- `examples/libopforge_in_memory.rs`
- `examples/libopforge_filesystem.rs`
- `documentation/libopforge-developer-guide.md`

still teach the API through large `OwnedAssemblerConfig` literals.

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

### 4.4 `unstable` is too flat

`unstable` currently aggregates engine-facing helpers, formatter exports, and
LSP entrypoints in one broad namespace.

That is functionally acceptable, but aesthetically it weakens the message that
the stable surface is curated.

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

- Slice A is **partially complete**:
  - the main specification example now uses the correct borrowed builder call
    shape
  - the public embedding examples and most developer-guide snippets still lead
    with config literals rather than builders
- Slice B is **not started**
- Slice C is **not started in substance**:
  - borrowed-builder parity improved slightly through `opasm_package_path`
  - the broader parity gap remains
- Slice D is **not started**
- Slice E is **not started**
- Slice F remains **deferred**, but its urgency has increased as the execution
  config family has grown again

### 5.1 Slice A: make the intended ergonomic path visible

This is the highest-value, lowest-risk slice.

Work items:

- Switch the primary public examples to builder-first forms where practical.
- Keep one explicit config-literal example for advanced hosts, but do not make
  it the first thing developers see.
- Update the developer guide so its first in-memory and filesystem examples use
  the builder-oriented path rather than full `OwnedAssemblerConfig` literals.
- Add a short ownership-choice table to the developer guide:
  - borrowed `Assembler`
  - owned `AssemblerSession`
  - when to prefer each
- Make the public docs show the common path in fewer moving parts.

Recommended success condition:

- a new developer can copy a builder-oriented example without first learning the
  full grouped config literal structure.

Concrete target shape:

```rust
let report = AssemblerSession::builder("/virtual/main.asm")
    .input_base("/virtual/main")
    .source_provider(source_provider)
    .output_sink(output_sink)
    .execution_mode(ExecutionMode::Vm)
    .prepare()?
    .assemble()?;
```

This slice changes no semantics and preserves the existing type model.

### 5.2 Slice B: add semantic aliases for developer-facing intent

Introduce additive names that better match downstream mental models.

Primary recommendation:

- add `output_base(...)` as a preferred alias for `input_base(...)`

Rationale:

- the README already has to explain that `input_base` is really about output
  naming behavior,
- “output base” is the phrase most embedders will infer naturally from the
  behavior,
- this can be introduced as an additive builder/config convenience without
  breaking compatibility.

Secondary recommendations:

- consider an additive `portable` alias alongside `normalized` in:
  - `libopforge::opcore`
  - `libopforge::asm::opasm`
- keep existing “normalized” language in the deeper lockstep/specification docs
  where it describes comparison semantics rather than only the public module
  name
- prefer new docs to describe this view as the “portable contract” view.

Recommended success conditions:

- common docs can avoid explaining `input_base` as a conceptual exception,
- the portable/normalized distinction becomes visually obvious.

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
- output selection and naming
- diagnostics toggles

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

- add clearer builder-level convenience methods that express intent, such as:
  - `no_outputs()`
  - `default_outputs(bool)` remaining available for exact control

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

### 5.5 Slice E: restructure `unstable` into named shelves

Do not remove `unstable`.
Do make it easier to visually parse.

Recommended direction:

- `libopforge::unstable::engine`
- `libopforge::unstable::formatter`
- `libopforge::unstable::lsp`

Compatibility strategy:

- keep existing flat re-exports temporarily if needed,
- document the nested modules as the preferred unstable layout,
- deprecate flat forms later only if worthwhile.

Recommended success conditions:

- `unstable` feels like a deliberately organized overflow zone,
- advanced users can tell at a glance which family of unstable features they
  are touching.

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
- large breaking renames in the stable surface just to remove historical terms.

These would create more churn than polish at this stage.

## 7. Recommended Order

Implement in this order:

1. Slice A: docs/examples and ownership-choice presentation
2. Slice B: additive semantic aliases (`output_base`, `portable`)
3. Slice C: borrowed-builder parity
4. Slice E: `unstable` namespacing
5. Slice D: output-policy convenience vocabulary
6. Slice F only if the surface still feels too heavy after the earlier slices

Why this order:

- it delivers visible polish early,
- it preserves compatibility,
- it tests whether the main aesthetic pain is teaching/ergonomics rather than
  deeper type architecture.

## 8. Success Criteria

This plan succeeds if, after the early slices:

- the first example a developer sees is builder-first and clearly intentional,
- the borrowed and owned builders look like members of the same API family,
- the most common naming friction (`input_base`) has a friendlier path,
- `unstable` no longer reads like a second uncontrolled public API,
- the stable surface still looks architecturally explicit rather than magically
  flattened.

## 9. Short Version

The API does **not** need a redesign-from-scratch.

It needs:

- better teaching of the intended path,
- additive naming cleanup,
- builder parity,
- containment of the overflow surface.

The architecture is already the elegant part.
The next step is making the ergonomics feel as intentional as the layering.
