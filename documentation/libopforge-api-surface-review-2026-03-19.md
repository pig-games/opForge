# Review Report

## Findings

### RVW-2026-03-19-001

- Severity: medium
- File: `crates/opforge-lib/src/lib.rs:4083`
- Issue: The documented `vm-runtime-only` public-crate mode is not kept green by the facade test suite. Running `cargo test -p libopforge --features vm-runtime-only` fails because `public_processing_api_routes_core_failures_through_core_error` reaches for `::engine::editor_default_runtime_model()` instead of following the documented facade-visible runtime-model path.
- Why it matters: This weakens confidence in one of the crate's advertised feature modes and makes the public `processing` contract easier to regress than the documentation suggests. Downstream hosts enabling `vm-runtime-only` are likely to encounter breakage later than they should.
- Fix direction (one direction only; resolve competing options before finalizing): Update the failing processing test to construct its runtime model through `processing::HierarchyExecutionModel::from_registry(&registry::default_asm_registry())` and run the `libopforge` suite in a `vm-runtime-only` lane so the documented feature contract stays exercised.

### RVW-2026-03-19-002

- Severity: low
- File: `crates/opforge-lib/src/lib.rs:1066`
- Issue: The main stable entrypoints and config surfaces are exported without type- or method-level rustdoc. The module docs are good, but the concrete ergonomics surface users actually autocomplete against, including `Owned*Options`, `PrepareOptions`, `AssembleOptions`, `AssemblerBuilder`, `AssemblerSessionBuilder`, `Assembler`, and `PreparedAssembly*`, is largely undocumented inline.
- Why it matters: For downstream users working in docs.rs or IDE hover, the supported path is harder to discover than it should be. Important distinctions such as borrowed vs owned flows, `check()` side effects, `output_base` derivation, and when to choose builders versus grouped config require jumping out to the long-form guide instead of being understandable at the API point of use.
- Fix direction (one direction only; resolve competing options before finalizing): Add rustdoc to every public facade type plus the high-traffic builder/session methods, using a consistent pattern of "when to use this", default behavior notes, and links back to the developer-guide examples.

## Scope

Review of the current `libopforge` public facade with a developer-experience focus, centered on:

- public Rust API shape in [`crates/opforge-lib/src/lib.rs`](/Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/crates/opforge-lib/src/lib.rs)
- host-facing documentation in [`documentation/libopforge-developer-guide.md`](/Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/documentation/libopforge-developer-guide.md)
- current public-crate validation posture, including default and feature-flagged test lanes

## Testing Gaps

- The public crate has no external integration-test directory; validation lives entirely inside [`crates/opforge-lib/src/lib.rs`](/Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/crates/opforge-lib/src/lib.rs#L2445), so examples are checked from inside the crate with `use crate as libopforge` instead of from a true downstream-consumer context.
- Feature coverage is materially weaker than default-lane coverage. The default `cargo test -p libopforge` run passes, but `cargo test -p libopforge --features vm-runtime-only` currently fails.
- The developer guide is mechanically checked through included example files, which is strong, but the README's API narrative is not held to the same compile-backed standard.

## Residual Risks

- The facade is intentionally rich and module-first, but it currently exposes many overlapping entrypoints and config forms. Even with the current guide, new users may still hesitate between `Assembler`, `AssemblerSession`, grouped config structs, and free `prepare`/`assemble` helpers.
- Because most public-contract checks are colocated in one large unit-test module, small packaging or visibility regressions can remain harder to spot than they would be with a true downstream integration suite.

## Brief Summary

The `libopforge` facade is already in a solid place structurally: the module ownership story is clear, the guide is thoughtful, and the default public test suite is strong. The two biggest developer-experience follow-ups are to keep the documented feature matrix green, especially `vm-runtime-only`, and to move more of the public guidance into inline rustdoc on the concrete API types users actually touch.
