# Review Report

## Scope

Full current worktree review of opForge, explicitly not limited to the branch diff against main.

## Findings

### RVW-2026-03-28-001

- Severity: high
- File: crates/opforge-vm/src/output_model.rs:331; crates/opforge-vm/src/output_model.rs:357; crates/opforge-engine/src/lib.rs:503; crates/opforge-engine/src/lib.rs:539; crates/opforge-engine/src/lib.rs:571; crates/opforge-engine/src/lib.rs:616; crates/opforge-engine/src/lib.rs:1589; documentation/libopforge-specification.md:464; documentation/opForge-reference-manual.md:915
- Issue: Output path resolution does not confine relative artifact names to the selected output root. `resolve_output_path` preserves `..` segments when joining list and hex names against the base directory, `resolve_bin_path` returns explicit bin names without anchoring them to the resolved base at all, and `resolve_artifact_output_path` joins directive paths under `out_dir` without rejecting escapes.
- Why it matters: A source file or metadata block can make assembly write list, hex, bin, map, linker-output, or export-section artifacts outside the caller-selected `out_dir` even though the public contract says relative output filenames are anchored. In CLI, library, and FFI-hosted flows, that turns `out_dir` into a best-effort hint rather than a real containment boundary.
- Fix direction (one direction only; resolve competing options before finalizing): Replace the split helpers with a single normalized artifact-path resolver that anchors every relative output name to the resolved output root and rejects any normalized path that escapes that root before creating directories or files.

### RVW-2026-03-28-002

- Severity: medium
- File: crates/opforge-engine/src/lib.rs:347; crates/opforge-engine/src/lib.rs:353; crates/opforge-engine/src/lib.rs:356; crates/opforge-engine/src/lib.rs:2216
- Issue: The artifact-backed editor runtime model cache is keyed only by artifact path and never invalidates when the bytes at that path change.
- Why it matters: Rebuilding or replacing the runtime package in place leaves editor tokenization, parsing, and diagnostic behavior bound to stale model data until the process restarts. In a normal regenerate-and-retry workflow, the editor can silently disagree with the current on-disk artifact.
- Fix direction (one direction only; resolve competing options before finalizing): Store file identity with each cached runtime model entry, such as modification metadata or a content fingerprint, and reload the model whenever the artifact at that path changes.

### RVW-2026-03-28-003

- Severity: medium
- File: crates/opforge-lsp/src/validation_runner.rs:34; crates/opforge-lsp/src/validation_runner.rs:67; crates/opforge-lsp/src/session.rs:919; crates/opforge-lsp/src/session.rs:930; crates/opforge-lsp/src/session.rs:939
- Issue: LSP validation workers invoke the external validator with blocking `Command::output()` and no timeout or cancellation path, while the session hard-caps active validations at two worker threads.
- Why it matters: A hung validator child permanently consumes one of the two validation slots, and two hung validations stop all subsequent diagnostics from being scheduled for the rest of the session.
- Fix direction (one direction only; resolve competing options before finalizing): Launch validation as a killable child process with a hard deadline or cancellation path, and tie `active_validations` decrement to a guard so worker capacity is restored on every exit path.

## Testing Gaps

- No regression test rejects `..` traversal from `.meta.output.list`, `.meta.output.hex`, `.meta.output.bin`, `.mapfile`, `.exportsections`, or linker output directives when `out_dir` is set.
- No test rewrites an existing runtime artifact at the same path and verifies `editor_default_runtime_model_for_dir` reloads updated bytes instead of reusing the cached model.
- No end-to-end LSP test simulates a hung validator child and verifies validation capacity recovers while pending documents still receive diagnostics.

## Residual Risks

- The terminal context shows `cargo test --workspace --all-targets`, `cargo clippy --workspace --all-targets -- -W clippy::all`, and `cargo check --workspace --all-features` succeeding, so the remaining risk is concentrated in path-boundary and long-lived-session behavior rather than broad build health.
- This was a full non-diff review, but it was still sample-based across a large multi-crate workspace. I did not exhaustively audit every CPU-family encoding path or every unsafe FFI lifetime contract in `crates/opforge-ffi/src/lib.rs`.
- Runtime artifact discovery still depends on process working directory for the artifact-backed editor path. That behavior appears intentional, but it remains operationally sensitive across CLI, editor, and embedding environments.

## Brief Summary

The current worktree is broadly healthy, but three material risks remain. The highest-severity issue is output-path confinement: callers can select `out_dir`, yet relative artifact names can still escape that directory. Two medium-severity correctness risks remain in long-lived tooling flows: the artifact-backed editor runtime model does not refresh when its package file is replaced in place, and LSP diagnostics can stall indefinitely if validator child processes hang.
