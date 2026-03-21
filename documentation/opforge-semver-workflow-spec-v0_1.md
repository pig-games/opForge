# opForge Semantic Versioning Workflow Spec v0.1

## Summary

This document proposes a repository-native semantic-versioning workflow for the
split `libopforge` workspace.

The proposal has three goals:

1. make version bumps intentional rather than ad hoc,
2. tie version changes to explicit public compatibility contracts,
3. make plans, reviews, and releases carry version-impact decisions as first
   class workflow artifacts.

It is designed for the current branch state, where:

- `libopforge` and `cli-core` are already at `0.9.5`,
- most split component crates are still at `0.1.0`,
- some surfaces are documented as stable host surfaces,
- some lower-level crates are still effectively internal implementation crates.

## Problem

The current workspace has meaningful public surfaces, but it does not yet have
an explicit semver policy that answers:

- which crates are treated as stable public contracts,
- which crates are still internal/transitional,
- when a change requires a version bump,
- which version component should change,
- how version decisions are reviewed before release.

Without that policy, version numbers risk becoming either:

- too sticky to communicate real compatibility,
- or too noisy to be useful.

## Goals

- [x] Define a compatibility-first semver policy for the workspace.
- [x] Distinguish stable public crates from internal implementation crates.
- [x] Define version-bump criteria for major, minor, and patch changes.
- [x] Define a pre-1.0 policy that is still idiomatic for Rust/Cargo crates.
- [x] Require version-impact classification during planning and review.
- [x] Separate crate semver from ABI/schema/container versioning.
- [x] Provide a release workflow that can be automated later.

## Non-Goals

- [x] This spec does not declare that every current crate is ready for `1.0.0`.
- [x] This spec does not require all workspace crates to share one lockstep version forever.
- [x] This spec does not replace explicit ABI/schema versions for VM contracts,
  file formats, or package/container layouts.
- [x] This spec does not define exact release-note wording.
- [x] This spec does not prescribe a specific third-party release tool.

## Invariants / Constraints

- The active worktree [AGENTS.md](/Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/AGENTS.md)
  rules remain binding for all plan, review, and release work.
- Version changes must be driven by compatibility impact, not by implementation
  effort alone.
- Crate semver applies to that crate's documented contract surface, not to
  unspecified internals.
- ABI, wire-protocol, and container-format versions remain separate from crate
  semver and must be bumped under their own contract rules.
- A docs-only, test-only, or internal-only change that does not alter any
  supported contract should not force a version bump.

## Behavioral Contract

### 1. Component classes

Every crate must be classified into one of these semver classes.

#### Class A: stable public crates

These crates own documented external contracts and should be versioned as true
public APIs.

Current likely members:

- `libopforge`
- `opforge-ffi`
- `opforge-cli`
- `opforge-cli-core`
- `opforge-formatter`
- `opforge-lsp`

Class-A crates should eventually graduate to `1.x.y` once their contracts are
considered intentionally stable.

#### Class B: internal-but-reusable crates

These crates have meaningful APIs, but their intended audience is mainly
opForge itself or advanced contributors rather than normal downstream users.

Current likely members:

- `opforge-core`
- `opforge-asm`
- `opforge-engine`
- `opforge-registry`
- `opforge-families`
- `opforge-vm`
- `opforge-package`
- `opforge-types`

These may remain on `0.x.y` longer, but they should still follow compatibility
discipline relative to their direct dependents.

### 2. Contract ownership

Versioning decisions are made against owned compatibility contracts, not against
raw code churn.

Examples:

- `libopforge` owns the documented Rust host facade described in
  [README.md](/Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/README.md),
  [libopforge Developer Guide](/Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/documentation/libopforge-developer-guide.md),
  and
  [libopforge Architecture Specification](/Users/erik/Code/Retro/opForge/worktrees/libopforge-lib/documentation/libopforge-specification.md).
- `opforge-ffi` owns the exported C ABI and header contract.
- `opforge-cli` and `opforge-cli-core` own CLI flags, defaults, user-visible
  behavior, and output contract expectations described in the reference manual
  and help text.
- `opforge-lsp` owns its protocol-facing LSP behavior.
- Internal crates own only the explicitly documented or intentionally exposed
  Rust APIs they publish to sibling crates.

### 3. Version-impact declaration

Every implementation plan, remediation plan, and release-focused review should
include a `Version Impact` section with:

- affected component(s),
- impact class: `none`, `patch`, `minor`, or `major`,
- the owned contract being evaluated,
- a one-paragraph rationale.

This is a workflow requirement, not an optional release-time add-on.

### 4. Recommended repository structures

Add a small semver policy layer:

- `documentation/opforge-semver-workflow-spec-v0_1.md`
- `references/workflow/semver-policy-guide.md`
- `templates/version-impact-template.md`
- `agents/semver-impact-reviewer.md`

Optional later automation:

- `scripts/workflow/check_version_impact.py`
- `release/semver-components.toml`

`release/semver-components.toml` should record, per crate:

- semver class,
- contract owner doc(s),
- current stability phase (`pre1` or `stable1`),
- whether the crate is expected to release independently or with the normal
  opForge release train.

### 5. Recommended release workflow

For any release-bearing change:

1. classify affected components and owned contracts,
2. declare version impact in the plan,
3. have review validate whether the declared impact is too small, correct, or
   too large,
4. run a semver-impact reviewer before release prep,
5. update crate versions and release notes together,
6. tag the release only after version-impact validation passes.

### 6. Version-component criteria

#### 6.1 Post-1.0 crates (`1.x.y` and above)

For stable public crates that have graduated to `1.x.y`, use standard semver:

- `MAJOR`:
  - any breaking public API/ABI/protocol/CLI contract change,
  - removal or rename of public items,
  - changed meaning of existing inputs/options/fields,
  - changed defaults that can alter expected host behavior,
  - incompatible output or protocol contract changes,
  - incompatible C ABI/header changes.
- `MINOR`:
  - backward-compatible new public APIs,
  - additive new CLI flags or protocol capabilities,
  - additive enum variants only when the type is explicitly designed for that,
  - new optional behavior that does not break existing callers.
- `PATCH`:
  - backward-compatible bug fixes,
  - performance improvements with unchanged contract,
  - diagnostic wording improvements that do not alter a supported machine-read
    contract,
  - internal refactors with no supported contract change.

#### 6.2 Pre-1.0 crates (`0.x.y`)

For crates that remain pre-1.0, use the Rust/Cargo-compatible convention:

- `MINOR` is the compatibility boundary.
- `PATCH` is for backward-compatible changes.

That means:

- breaking change: `0.x.y` -> `0.(x+1).0`
- backward-compatible additive feature: `0.x.y` -> `0.x.(y+1)`
- backward-compatible fix: `0.x.y` -> `0.x.(y+1)`

This intentionally treats `0.x` as the major-equivalent boundary because that
matches Cargo's compatibility behavior for pre-1.0 crates.

### 7. Component-specific bump criteria

#### 7.1 `libopforge`

`libopforge` should bump for changes to the curated stable host facade.

Breaking examples:

- removing or renaming stable modules,
- changing stable type names or function signatures,
- tightening input requirements in a way that invalidates existing host code,
- changing builder/session behavior in a way that breaks current call patterns,
- changing public output-routing semantics in an incompatible way.

Compatible examples:

- adding new stable modules,
- adding new builder methods without breaking existing calls,
- additive diagnostics/reporting helpers,
- new optional execution or output modes.

#### 7.2 `opforge-ffi`

`opforge-ffi` should version against the C ABI and header.

Breaking examples:

- changing `repr(C)` layout,
- removing exported functions,
- changing ownership/freeing rules,
- changing callback contracts incompatibly.

Compatible examples:

- adding new exported functions,
- additive request fields when the ABI contract explicitly allows them,
- bug fixes that preserve header and ownership expectations.

#### 7.3 `opforge-cli` and `opforge-cli-core`

These should version against user-visible CLI behavior.

Breaking examples:

- removing or renaming flags,
- changing output-file defaults incompatibly,
- changing exit-code semantics,
- changing output formats in ways that break normal scripted consumption.

Compatible examples:

- additive new flags,
- additive new output modes,
- bug fixes that make behavior match existing docs.

#### 7.4 `opforge-lsp`

This should version against protocol-facing behavior and advertised capability
contracts.

Breaking examples:

- removing or renaming supported requests,
- changing payload shapes incompatibly,
- withdrawing previously documented capabilities.

Compatible examples:

- additive capabilities,
- additive optional fields,
- bug fixes that preserve existing client compatibility.

#### 7.5 internal crates

Internal crates should still version honestly, but only against their actual
direct Rust API contracts.

If a crate is not yet intended as a downstream dependency, keep it on `0.x.y`
and use:

- `minor` bump for breaking API changes,
- `patch` bump for compatible additions and fixes.

### 8. Graduation to 1.0

A Class-A crate should only move to `1.0.0` when all of the following are true:

- its owned contract is explicitly documented,
- its main compatibility promises are intentional rather than provisional,
- its normal validation lanes cover the supported surface,
- release notes and migration notes exist for user-facing changes,
- the team is willing to treat breaking changes as exceptional rather than
  routine.

Recommendation for this branch:

- do not force all crates to `1.0.0` immediately,
- first adopt the workflow and impact classification,
- then graduate `libopforge` and `opforge-ffi` once the public facade and C ABI
  are considered stable enough to defend.

## Boundary Cases

- Docs-only or comment-only changes:
  - `Version Impact: none`
- Internal refactor with no contract change:
  - `Version Impact: none`
- Compatible bug fix in a pre-1.0 public crate:
  - patch bump only
- Additive public API in a pre-1.0 public crate:
  - patch bump only
- Breaking public API in a pre-1.0 public crate:
  - minor bump
- Additive public API in a post-1.0 crate:
  - minor bump
- Breaking C ABI change in `opforge-ffi`:
  - major bump if `>=1.0`, minor bump if still `0.x`
- VM package/container schema change:
  - bump schema/container version under its own spec; crate semver may also
    bump if the crate's owned contract changes
- Diagnostic wording tweak:
  - normally patch or none, unless the wording is part of a documented
    machine-consumed contract

## Acceptance Criteria

- [x] The repo has a documented policy for classifying crates by semver class.
- [x] The repo has explicit bump criteria for pre-1.0 and post-1.0 crates.
- [x] The policy distinguishes crate semver from ABI/schema versioning.
- [x] Plans and reviews are expected to carry version-impact declarations.
- [x] The proposal defines concrete next repository structures to support this.
- [x] The proposal gives component-specific criteria for `libopforge`, `ffi`,
  CLI, LSP, and internal crates.

## Validation Expectations

This spec is ready to drive follow-on planning if the next implementation plan
does all of the following:

- adds the semver policy guide/template/reviewer structures,
- updates workflow rules so plans and release work must carry version impact,
- defines an initial classification for every workspace crate,
- decides whether `libopforge` and `opforge-ffi` stay pre-1.0 or begin 1.0
  graduation planning.

## Open Questions

None for the proposal itself. The main follow-on decision is strategic rather
than ambiguous: whether to keep `libopforge`/`opforge-ffi` in disciplined
pre-1.0 mode for another release cycle, or intentionally begin a `1.0.0`
readiness pass.
