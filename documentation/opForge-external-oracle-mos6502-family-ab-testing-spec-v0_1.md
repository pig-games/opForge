# opForge MOS 6502 Family External-Oracle A/B Testing Spec (v0.1)

## Summary
This specification defines the `mos6502`-family version of opForge's
external-oracle A/B testing workflow, using `64tass` as the first oracle for
the currently shipped MOS-family CPU surface: `m6502`, `65c02`, `65816`, and
`45gs02`.

The workflow remains metadata-driven, opt-in, and separate from
`examples/reference`. It compares shared-subset success cases by normalized
final bytes, compares shared-subset failure cases by normalized error class,
and keeps intentional `opForge` versus `64tass` differences visible through
documented-divergence fixtures rather than silent skips.

This specification translates the generic Option B external-oracle contract into
the concrete `mos6502` family boundary and fixture shape that future plans and
implementation work can execute without having to rediscover family ids, CPU
coverage, corpus layout, or oracle-scope rules.

## Problem
The repository already contains:

- a generic external-assembler A/B testing proposal centered on a reusable,
  Rust-native harness
- a proven `vasm`-backed implementation path for Motorola 680x0
- a family-expansion plan showing how the harness scales from a narrow first
  slice to per-CPU and per-profile corpora

What it does not yet contain is a corresponding specification for the
`mos6502` family using `64tass` as the oracle.

Without a family-specific specification, follow-on planning and implementation
work remain underspecified in five ways.

First, the intended CPU scope is unclear. opForge ships `m6502`, `65c02`,
`65816`, and `45gs02` under the `mos6502` family, but there is no artifact
stating whether the external-oracle workflow should cover only the baseline CPU
or the full shipped family.

Second, the shared-subset boundary is unclear. opForge is intentionally
64tass-inspired in expressions, macros, and directive naming, but the two tools
are not interchangeable. Without an explicit shared-subset contract, fixture
authors could accidentally treat 64tass-only directives, opForge-only features,
or output-container quirks as regressions.

Third, the corpus shape is unclear. The generic proposal defines a reusable
metadata model, and the 680x0 expansion plan proves that per-CPU manifests scale
well, but no artifact states the expected `mos6502` layout, naming, or CPU
mapping.

Fourth, the comparison boundary is unclear for MOS-family output styles. The
6502 ecosystem often mixes raw binaries, load-address-prefixed images, and
assembler-specific pseudo-instruction behaviors. Without a specification, the
harness could accidentally compare container bytes instead of semantic emitted
program bytes.

Fifth, family-specific divergence handling is unclear. `65816` state control,
`45gs02`/`4510` target selection, and directive/macro differences need explicit
rules so that intentional gaps stay visible without destabilizing the shared
parity corpus.

## Goals
- [ ] `REQ-EXTAB6502-001`: Add a dedicated external-oracle A/B workflow for the
      `mos6502` family that remains separate from the existing
      `examples/reference` golden-reference workflow.
- [ ] `REQ-EXTAB6502-002`: Use `64tass` as the first external oracle for the
      `mos6502` family.
- [ ] `REQ-EXTAB6502-003`: Preserve the existing `mos6502` family id and the
      currently shipped CPU identities `m6502`, `65c02`, `65816`, and
      `45gs02` as the family surface governed by this workflow.
- [ ] `REQ-EXTAB6502-004`: Define a per-CPU manifest layout under
      `examples/ab/mos6502/64tass/` that keeps the corpus reviewable,
      deterministic, and scalable across the family.
- [ ] `REQ-EXTAB6502-005`: Compare shared-subset success fixtures primarily by
      normalized final bytes, not by listings, maps, symbol tables, or raw
      container formats.
- [ ] `REQ-EXTAB6502-006`: Compare shared-subset failure fixtures by a bounded,
      documented normalized error taxonomy rather than raw stderr text.
- [ ] `REQ-EXTAB6502-007`: Support explicit documented-divergence fixtures so
      known `opForge` versus `64tass` differences remain visible without being
      turned into ad hoc skips.
- [ ] `REQ-EXTAB6502-008`: Keep the workflow opt-in and environment-gated so
      the default local test path and default CI/workspace path do not require
      `64tass` to be installed.
- [ ] `REQ-EXTAB6502-009`: Keep the oracle adapter boundary and metadata model
      family-agnostic so the same harness architecture remains reusable beyond
      `mos6502`.
- [ ] `REQ-EXTAB6502-010`: Seed the corpus from existing `examples/mos6502/`
      sources wherever possible so family coverage grows from repo-backed
      examples instead of ad hoc new fixtures.
- [ ] `REQ-EXTAB6502-011`: Make mismatch reporting actionable by emitting
      per-fixture summaries that name the fixture, CPU, oracle, compare mode,
      and concrete difference.

## Non-Goals
- [ ] `NREQ-EXTAB6502-001`: Replace opForge's existing
      `examples/reference` workflow with `64tass`.
- [ ] `NREQ-EXTAB6502-002`: Treat `64tass` as the authoritative behavioral
      source for opForge's `mos6502` family.
- [ ] `NREQ-EXTAB6502-003`: Require parity for opForge-only directives,
      macros, pseudo-instructions, VM-related behavior, or source forms that are
      intentionally outside the declared shared subset.
- [ ] `NREQ-EXTAB6502-004`: Compare listing text, map files, symbol tables,
      relocatable objects, or other non-byte artifacts in v0.1.
- [ ] `NREQ-EXTAB6502-005`: Require project-level include trees, macro
      libraries, or external downloads at test time.
- [ ] `NREQ-EXTAB6502-006`: Redesign the generic external-oracle harness,
      registry architecture, or existing `mos6502` family implementation.
- [ ] `NREQ-EXTAB6502-007`: Promise coverage for non-shipped MOS-family targets
      such as `65ce02`, `65el02`, or other 64tass-supported CPUs that opForge
      does not currently ship.
- [ ] `NREQ-EXTAB6502-008`: Mix milestone ordering, commit slicing, or plan
      sequencing into this specification.

## Invariants / Constraints
- The active worktree `AGENTS.md` workflow and execution rules remain binding
  for any plan or implementation derived from this specification.
- The current opForge golden-reference workflow remains the primary oracle for
  opForge-owned examples and references.
- External-oracle fixtures for this family must live in a dedicated corpus under
  `examples/ab/mos6502/64tass/` and must not be merged into
  `examples/reference`.
- The family id remains `mos6502`.
- The governed canonical CPU ids remain `m6502`, `65c02`, `65816`, and
  `45gs02`.
- A fixture is eligible for A/B testing only if it belongs to the declared
  shared subset for the selected CPU and oracle profile.
- The default comparison contract is bytes-first for success cases and
  normalized-error-class-first for failure cases.
- Documented divergences are first-class fixture metadata, not silent skips.
- The harness must stay skip-safe when the required oracle executable is
  unavailable.
- The metadata model and adapter boundary must remain generic enough to support
  non-`mos6502` families later without redesign.
- Existing `examples/mos6502/` sources are the primary seed material for curated
  corpus derivation.
- Output normalization must compare semantic emitted bytes, not incidental
  container bytes such as a load-address prefix when that prefix is not part of
  the declared compare contract.

## Behavioral Contract

### Family identity and corpus layout
- The family id is `mos6502`.
- The oracle id is `64tass`.
- The canonical corpus root is `examples/ab/mos6502/64tass/`.
- The default manifest layout is one manifest root per CPU:
  - `examples/ab/mos6502/64tass/m6502/fixtures.toml`
  - `examples/ab/mos6502/64tass/65c02/fixtures.toml`
  - `examples/ab/mos6502/64tass/65816/fixtures.toml`
  - `examples/ab/mos6502/64tass/45gs02/fixtures.toml`
- Each manifest root may contain:
  - `positive/*.asm`
  - `negative/*.asm`
  - `documented_divergence/*.asm`
- The manifest format is metadata-driven and must include at least:
  - `manifest_version`
  - `family`
  - `oracle`
  - `oracle_profile`
  - optional `cpu_profile`
  - default `expected_outcome`
  - default `compare_mode`
  - one or more `[[fixtures]]` entries with per-fixture overrides
- Each fixture record must support at least:
  - `id`
  - `cpu`
  - `path`
  - optional per-fixture `expected_outcome`
  - optional per-fixture `compare_mode`
  - required `documented_divergence_kind` for
    `expected_outcome = documented_divergence`
  - required `expected_opforge_status` for
    `expected_outcome = documented_divergence`
  - required `expected_oracle_status` for
    `expected_outcome = documented_divergence`
  - optional `expected_opforge_error_class`
  - optional `expected_oracle_error_class`
  - required `documented_divergence_reason` for
    `expected_outcome = documented_divergence`
  - optional extra oracle arguments
  - optional normalization hints

### CPU coverage and mapping
- The governed CPU surface is the currently shipped opForge `mos6502` family:
  - `m6502`
  - `65c02`
  - `65816`
  - `45gs02`
- The workflow must treat each CPU as a distinct corpus surface, not as one
  monolithic family manifest.
- The adapter must map each opForge CPU to an explicit `64tass` target mode that
  exposes the intended CPU surface rather than silently down-leveling to a
  different target.
- The v0.1 CPU-to-oracle mapping is:

| opForge CPU | Required `64tass` target | Required default `oracle_profile` |
| --- | --- | --- |
| `m6502` | `6502` | `tass_6502_flat_binary` |
| `65c02` | `65c02` | `tass_65c02_flat_binary` |
| `65816` | `65816` | `tass_65816_flat_binary` |
| `45gs02` | `45gs02` | `tass_45gs02_flat_binary` |

- In v0.1, all four default profiles are semantic aliases for the same compare
  contract: invoke the selected `64tass` CPU target, emit a raw binary artifact
  suitable for semantic-byte normalization, and compare only the emitted program
  bytes.
- For `45gs02`, the adapter must select a `64tass` target that exposes the
  `45GS02` opcode surface rather than silently falling back to a narrower `4510`
  or baseline 6502 mode.
- For `65816`, the workflow may use `cpu_profile` or fixture-level metadata when
  needed to keep accumulator/index width state, direct-page assumptions, or
  databank-sensitive behavior explicit and deterministic.

### Shared-subset requirement
Only fixtures intentionally written in the shared subset may enter this corpus.

A shared-subset `mos6502` fixture:

- uses only syntax that both opForge and the selected `64tass` target are
  intended to parse
- uses only directives and output modes explicitly allowed by the selected
  profile
- does not depend on opForge-only directives, macros, VM hooks, or output
  behaviors
- does not depend on `64tass`-only directives, pseudo-instructions, or target
  quirks unless the fixture is explicitly marked as a documented divergence
- keeps any `65816` width or bank-state assumptions explicit in source or
  metadata instead of relying on ambient tool defaults

This means the external-oracle corpus is not a mirror of the normal
`examples/mos6502/` tree. It is a curated shared-subset corpus derived from it.

### Comparison modes
#### Success cases
For `expected_outcome = success`, the default compare mode is `bytes`.

The workflow must:

- run opForge on the fixture into a temporary output directory
- run `64tass` on the same fixture into a temporary output directory
- normalize the produced artifact into a flat comparable byte stream
- fail if the normalized byte streams differ

The first comparison contract is semantic-byte-focused. It does not compare:

- listing text
- map output
- symbol tables
- relocatable object structure
- incidental output-container bytes that are outside the declared compare
  contract

If the selected `64tass` output mode prepends a load address or other container
wrapper, the normalization layer must strip or otherwise normalize that wrapper
before comparing semantic emitted bytes.

#### Error cases
For `expected_outcome = error`, the default compare mode is `error_class`.

The workflow must:

- run both assemblers
- require both to fail
- normalize each failure into a stable opForge-side error taxonomy
- fail if the normalized classes differ

The workflow must not require raw stderr strings to match.

Example normalized classes include:

- `unknown-mnemonic`
- `illegal-addressing-mode`
- `unsupported-cpu-feature`
- `branch-out-of-range`
- `value-out-of-range`
- `syntax-error`
- `missing-operand`
- `wrong-operand-count`

The taxonomy may widen over time, but it must stay bounded and documented.

#### Documented divergence cases
For `expected_outcome = documented_divergence`, the workflow must:

- run both tools
- capture the structured mismatch
- report it in summaries
- not fail the suite unless the observed divergence no longer matches the
  documented divergence contract

This keeps intentional differences visible without turning them into silent
skips.

Documented-divergence fixtures must be machine-checkable. In v0.1 they must
declare:

- `documented_divergence_kind`
- `expected_opforge_status`
- `expected_oracle_status`
- `documented_divergence_reason`

The allowed `documented_divergence_kind` values are:

- `opforge_error_oracle_success`
- `opforge_success_oracle_error`
- `byte_mismatch`
- `error_class_mismatch`

Additional requirements:

- if `expected_opforge_status = error`, the fixture must declare
  `expected_opforge_error_class`
- if `expected_oracle_status = error`, the fixture must declare
  `expected_oracle_error_class`
- if `documented_divergence_kind = error_class_mismatch`, the fixture must
  declare both normalized error classes
- if `documented_divergence_kind = byte_mismatch`, the harness only needs to
  verify that both tools succeed and the normalized bytes still differ; a full
  byte match becomes a reclassification candidate

### Oracle adapter boundary
The harness must expose a generic oracle adapter interface with at least:

- adapter identity
- supported families
- required command availability check
- command-line construction for a fixture
- output artifact discovery
- stdout/stderr capture
- output normalization hooks
- error normalization hooks

The concrete adapter for this specification is `64tass`.

The opt-in contract should mirror the existing external-oracle workflow shape:

- the dedicated `64tass` workflow is disabled by default
- enabling it requires an explicit environment gate such as
  `OPFORGE_EXTERNAL_ORACLE_64TASS=1`
- the adapter defaults to looking for `64tass` on `PATH`
- contributors and CI may override the executable path with an explicit variable
  such as `OPFORGE_64TASS_BIN`

### Reporting contract
When a fixture mismatches, the workflow must emit:

- fixture id
- family
- cpu
- oracle id
- compare mode
- observed status for opForge
- observed status for the oracle
- normalized mismatch summary
- paths to captured outputs or stderr logs

For byte mismatches, the report should include:

- opForge byte length
- oracle byte length
- a short hex diff or first-differing-offset summary

For error mismatches, the report should include:

- normalized opForge error class
- normalized oracle error class
- short excerpts from captured diagnostics

## Boundary Cases
- If the `64tass` executable is unavailable, the workflow must skip with an
  explicit reason instead of failing as a regression.
- If a manifest or fixture omits required metadata, the workflow must fail as a
  harness error.
- If both tools succeed but no comparable output artifact is found, the workflow
  must fail as a harness error.
- If one tool succeeds and the other fails for a `success` fixture, the workflow
  must fail.
- If one tool fails and the other succeeds for an `error` fixture, the workflow
  must fail.
- If a documented-divergence fixture unexpectedly matches fully, the workflow
  may report that fixture as a candidate for reclassification.
- `64tass`-only directives, macro facilities, output shims, or pseudo
  instructions must not enter the shared-parity corpus unless they are marked as
  documented divergences.
- opForge-only directives, VM surfaces, or source conveniences must not enter
  the shared-parity corpus.
- `65816` fixtures that depend on width-state or bank-state behavior must make
  that state explicit rather than relying on ambient defaults from either tool.
- `45gs02` fixtures must not silently run against a narrower oracle target than
  the `45GS02` surface they are meant to validate.

## Acceptance Criteria
- [ ] `AC-EXTAB6502-001`: The repository can host a dedicated corpus under
      `examples/ab/mos6502/64tass/` without changing or widening
      `examples/reference`.
- [ ] `AC-EXTAB6502-002`: The fixture metadata model can represent per-CPU
      manifests for `m6502`, `65c02`, `65816`, and `45gs02`, including
      success, error, and documented-divergence outcomes.
- [ ] `AC-EXTAB6502-003`: A `64tass` adapter can run curated shared-subset
      success fixtures and compare normalized final bytes for each governed CPU.
- [ ] `AC-EXTAB6502-004`: The same workflow can run curated shared-subset
      negative fixtures and compare normalized error classes for each governed
      CPU where negative coverage exists.
- [ ] `AC-EXTAB6502-005`: Intentional `opForge` versus `64tass` differences can
      be represented as documented-divergence fixtures that remain visible but
      non-failing while their documented contract still matches observed
      behavior.
- [ ] `AC-EXTAB6502-006`: The workflow is opt-in, environment-gated, and skips
      cleanly when `64tass` is unavailable.
- [ ] `AC-EXTAB6502-007`: Mismatch reports are structured enough to identify the
      fixture, CPU, oracle, compare mode, and concrete difference without a
      manual rerun.
- [ ] `AC-EXTAB6502-008`: The normal reference workflow remains unaffected, and
      `cargo test -p asm examples_match_reference_outputs` stays clean when the
      oracle workflow is disabled or unavailable.
- [ ] `AC-EXTAB6502-009`: The metadata model and adapter boundary remain generic
      enough that another family/oracle pairing could reuse the same harness
      architecture without redesign.

## Validation Expectations
- Add fixture-schema validation tests for `mos6502` manifests.
- Add adapter availability tests that skip cleanly when `64tass` is not
  configured.
- Add curated positive shared-subset fixtures for `m6502`, `65c02`, `65816`,
  and `45gs02`, seeded from existing `examples/mos6502/` sources where
  possible.
- Add curated negative shared-subset fixtures for CPUs where the shared subset
  includes deterministic cross-tool failure behavior.
- Add at least one documented-divergence fixture proving the divergence path is
  visible but non-failing.
- Verify that byte normalization removes any output-container differences that
  are outside the declared compare contract.
- Verify that running the normal example/reference workflow is unaffected by the
  external-oracle workflow being absent.
- Verify that the dedicated external-oracle command can run in CI or locally
  with explicit `64tass` installation/configuration while remaining harmless
  when not enabled.
- Keep the active worktree `AGENTS.md` workflow and execution rules binding for
  all follow-on planning and implementation work derived from this
  specification.

## Open Questions
None for v0.1. Exact `64tass` command-line target tokens and output-flag details
may be captured in the follow-on implementation plan as long as they preserve
the CPU mapping, shared-subset boundary, and opt-in contract defined here.
