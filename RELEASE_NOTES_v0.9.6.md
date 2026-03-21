# opForge v0.9.6 Release Notes

## Scope

This release promotes the split `libopforge` workspace to the next public
release train, expands the stable host-facing facade, and hardens release and
cross-platform validation for the new library-oriented surface.

## Highlights

- Promoted the published `libopforge` and `cli-core` surface from `0.9.5` to
  `0.9.6`.
- Landed the multi-crate workspace split behind a curated stable `libopforge`
  facade and shared `cli-core` command surface.
- Expanded the public Rust and C host-facing APIs for assembly, diagnostics,
  processing, registry, lockstep, formatter, session, and prepared-session
  workflows.
- Strengthened release engineering with `release-ffi` packaging, ABI/export
  coverage, panic-boundary smoke tests, and broader build-matrix validation.
- Normalized public dependency/source-map path rendering across platforms so
  Windows release validation matches the stable host contract.

## Added

- Stable `libopforge` documentation for embedding, diagnostics/fixits,
  execution modes/lockstep, and FFI usage patterns.
- Release-FFI smoke and export-surface validation in both release and CI paths.
- Branch-local workflow assets for plan/review/spec/closure quality gates in
  this worktree.

## Changed

- The repository is now organized as a split workspace with dedicated crates
  for core language semantics, assembler behavior, orchestration, families,
  formatter, LSP, CLI, FFI, and the published facade.
- `cli-core` now owns the visible CLI version/build-profile reporting used by
  `opforge --version` and related command output.
- Host-facing dependency output and source-origin reporting now use stable
  slash-form path text instead of leaking platform-specific separator or
  verbatim-path forms.

## Fixed

- Windows release validation no longer fails on mixed-separator or canonical
  path-form drift in public `libopforge` dependency/source-map outputs.
- Release packaging now verifies the unwind-safe FFI build profile and shipped
  library exports more directly.
- LSP and assembly host flows in the promoted workspace now share the same
  stable error/reporting contracts more consistently.

## Validation

Release validation for this branch included:

- `cargo fmt --all --check`
- `cargo clippy -- -D warnings`
- `cargo audit`
- `cargo test --locked`
- `cargo test -p libopforge public_ --lib`

## Upgrade Notes

- External Rust consumers targeting the stable host surface should depend on
  `libopforge` `0.9.6`.
- CLI/version-facing surfaces now report `0.9.6` through `cli-core`.
- For release automation, continue using one release-notes file per tag and do
  not edit previously tagged release-note files.
