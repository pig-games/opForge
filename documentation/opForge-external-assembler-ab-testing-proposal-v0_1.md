# opForge External Assembler A/B Testing Proposal (v0.1)

## Summary
This proposal defines an external-oracle A/B testing workflow for opForge
assembler validation, starting with Motorola 680x0 comparison against an
existing third-party assembler such as `vasm`.

The goal is to add fast, automatable parity evidence for the shared,
intentionally overlapping subset between opForge and an external assembler
without turning the external tool into the source of truth for opForge
behavior.

The design has two equally important sides:

- the logical comparison contract:
  - what counts as comparable input
  - what outputs are compared
  - how divergences are classified
  - when mismatches should fail versus merely report
- the technical infrastructure contract:
  - how fixtures are organized
  - how external assemblers are invoked
  - how outputs and diagnostics are normalized
  - how the workflow stays opt-in, deterministic, and scalable across CPU
    families

The recommended fastest path is:

- a dedicated external-oracle corpus separate from the existing
  `examples/reference` golden workflow
- `vasm` as the first oracle adapter
- flat-binary byte comparison for success cases
- normalized diagnostic-class comparison for error cases
- environment-gated local/CI execution so the workflow does not depend on
  third-party tool availability by default

## Problem
opForge currently has strong internal validation through unit tests, example
fixtures, golden references, and native/VM parity checks. Those workflows prove
internal consistency well, but they do not directly answer a different question:

“For source that should be compatible with an established external assembler,
does opForge behave the same way?”

That question matters for 680x0 bring-up because:

- compatibility expectations are often anchored in existing tools such as
  `vasm`
- parser and encoder bugs may still produce internally consistent but
  externally incompatible results
- diagnostics and legality rules benefit from an oracle on the intended shared
  subset

At the same time, a naive external comparison harness would create the wrong
incentives:

- it would overfit opForge to one external tool’s quirks
- it would conflate intentional syntax differences with regressions
- it would destabilize the current golden-reference workflow
- it would not scale well to other CPU families if built as a one-off `vasm`
  script

The proposal therefore needs to define a streamlined end-to-end approach
that is:

- practical to add quickly
- honest about scope
- reusable across families
- strict enough to catch real compatibility regressions

## Goals
- [ ] `REQ-EXTAB-001`: Add a dedicated external-oracle A/B workflow separate
      from the current `examples/reference` golden-reference workflow.
- [ ] `REQ-EXTAB-002`: Start with a `vasm` oracle adapter for Motorola 680x0
      comparison.
- [ ] `REQ-EXTAB-003`: Define a fixture corpus format that captures CPU,
      oracle, expected outcome, comparison mode, and documented divergence
      metadata without embedding tool-specific logic in test code.
- [ ] `REQ-EXTAB-004`: Compare successful assemblies primarily by normalized
      produced bytes, not by listing text.
- [ ] `REQ-EXTAB-005`: Compare failing assemblies by normalized diagnostic
      class, not raw stderr text.
- [ ] `REQ-EXTAB-006`: Support explicit “documented divergence” fixtures so
      intentional differences are visible without failing the workflow.
- [ ] `REQ-EXTAB-007`: Keep the workflow opt-in and environment-gated so the
      default local test path and default CI path do not require third-party
      assemblers to be installed.
- [ ] `REQ-EXTAB-008`: Define a generic oracle-adapter boundary that allows
      later reuse for other external assemblers and CPU families.
- [ ] `REQ-EXTAB-009`: Keep the first working slice small enough to land fast,
      with `68000` and `68010` byte-parity fixtures before broader rollout.
- [ ] `REQ-EXTAB-010`: Make mismatch reporting actionable by emitting per-fixture
      compare summaries rather than opaque pass/fail text.

## Non-Goals
- [ ] `NREQ-EXTAB-001`: Replace opForge’s existing `examples/reference`
      workflow with an external assembler oracle.
- [ ] `NREQ-EXTAB-002`: Treat `vasm` or any external assembler as the
      authoritative behavioral source for opForge behavior.
- [ ] `NREQ-EXTAB-003`: Require byte or diagnostic parity for opForge-specific
      syntax, directives, aliases, or intentionally divergent semantics.
- [ ] `NREQ-EXTAB-004`: Compare listings, maps, object files, relocation tables,
      or symbol tables in the first slice.
- [ ] `NREQ-EXTAB-005`: Add emulator-style execution checks or runtime
      verification against CPU behavior.
- [ ] `NREQ-EXTAB-006`: Build a family-specific one-off harness that cannot be
      reused beyond 680x0.
- [ ] `NREQ-EXTAB-007`: Make the first slice depend on broad macro/include
      compatibility or full project-level assembly flows.
- [ ] `NREQ-EXTAB-008`: Require external network downloads at test time.

## Invariants / Constraints
- The current opForge golden-reference workflow remains the primary oracle for
  opForge-owned examples and references.
- External-oracle fixtures must live in a dedicated corpus separate from the
  ordinary example/reference fixtures.
- The external-oracle workflow must be skippable when the required external
  assembler is unavailable.
- A fixture is only eligible for A/B testing if it belongs to a declared shared
  subset for the selected family/oracle pair.
- Successful-case comparison is bytes-first. Listings and maps are not part of
  the default comparison contract in this proposal.
- Failing-case comparison is based on normalized error categories, not raw tool
  wording.
- The oracle adapter boundary must be family-agnostic even if the first adapter
  is `vasm` for 680x0.
- The workflow must support intentional divergences as first-class metadata, not
  as ad hoc test skips.

## Behavioral Contract

### Corpus structure
External-oracle A/B fixtures live in a dedicated corpus, for example:

- `examples/ab/<family>/<oracle>/positive/*.asm`
- `examples/ab/<family>/<oracle>/negative/*.asm`
- `examples/ab/<family>/<oracle>/*.toml`

Each assembly fixture has a sidecar metadata file or equivalent structured
record containing at least:

- `family`
- `cpu`
- `oracle`
- `oracle_profile`
- `expected_outcome`:
  - `success`
  - `error`
  - `documented_divergence`
- `compare_mode`:
  - `bytes`
  - `error_class`
  - `bytes_and_error_class` is reserved for future use
- optional expected divergence reason
- optional extra oracle arguments
- optional normalization hints

The metadata model must be generic enough to support non-680x0 families later.

### Shared-subset requirement
Only fixtures intentionally written in the shared subset may enter this corpus.

A shared-subset fixture:

- uses only syntax that both opForge and the selected oracle are intended to
  parse
- uses only directives and output modes explicitly allowed by the fixture
  profile
- does not depend on opForge-only aliases or external-assembler-only quirks
  unless marked as a documented divergence case

This means the external-oracle corpus is not a mirror of the normal opForge
examples tree.

### Comparison modes
#### Success cases
For `expected_outcome = success`, the default compare mode is `bytes`.

The workflow must:

- run opForge on the fixture into a temp output directory
- run the selected oracle on the same fixture into a temp output directory
- normalize the produced artifact into a flat comparable byte stream
- fail if the normalized byte streams differ

The first slice compares only final flat bytes. It does not compare:

- listing text
- map output
- symbol tables
- relocatable object structure

#### Error cases
For `expected_outcome = error`, the default compare mode is `error_class`.

The workflow must:

- run both assemblers
- require both to fail
- normalize each failure into a stable opForge-side error class taxonomy
- fail if the normalized classes differ

The workflow must not require raw stderr strings to match.

Example normalized classes include:

- `unknown-mnemonic`
- `illegal-addressing-mode`
- `unsupported-cpu-feature`
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

### Oracle adapter boundary
The harness must expose a generic oracle adapter interface with at least:

- adapter identity
- supported families
- required command availability check
- command-line construction for a fixture
- output artifact discovery
- stderr/stdout capture
- output normalization hooks
- error normalization hooks

The first concrete adapter should be `vasm` for Motorola 680x0.

The adapter boundary exists so later work can add, for example:

- another 68k assembler
- a Z80 assembler
- a 6502-family assembler

without rewriting the core compare workflow.

### Recommended fastest architecture
The best fastest path is:

1. A metadata-driven corpus.
2. A single generic test runner inside the existing Rust test suite.
3. A small adapter layer for `vasm`.
4. Environment-gated execution.
5. Byte comparison for success fixtures only in the first slice.

This is recommended over shell-script-only orchestration because:

- fixture parsing, temp-dir management, and failure reporting integrate better
  with the existing Rust test suite
- normalization logic is easier to type-check and reuse
- later-family and later-oracle scaling is cleaner

It is also recommended over a large plugin-style architecture because the first
slice needs to land quickly.

### Alternative approaches considered
#### Option A: Shell script only
Pros:

- fastest to prototype
- low initial code volume

Cons:

- weak type safety
- awkward fixture metadata handling
- poorer failure reporting
- less reusable across families

Conclusion:

- acceptable for one-off experimentation
- not recommended as the primary long-term contract

#### Option B: Rust-native generic harness with thin adapters
Pros:

- fits existing test infrastructure
- easy structured reporting
- reusable normalization code
- scalable across families

Cons:

- slightly more initial design work than a shell script

Conclusion:

- recommended best/fastest durable approach

#### Option C: Treat external oracle as another golden-reference producer
Pros:

- superficially aligns with existing reference concepts

Cons:

- conflates opForge-owned references with external parity checks
- increases risk of accidental oracle drift
- makes documented divergence handling awkward

Conclusion:

- not recommended

### Reporting contract
When a fixture mismatches, the workflow must emit:

- fixture id
- family
- cpu
- oracle id
- compare mode
- observed status for opForge
- observed status for oracle
- normalized mismatch summary
- paths to captured outputs or stderr logs

For byte mismatches, the report should include:

- opForge byte length
- oracle byte length
- a short hex diff or first differing offset summary

For error mismatches, the report should include:

- normalized opForge error class
- normalized oracle error class
- short excerpts from captured diagnostics

## Boundary Cases
- If the external oracle command is unavailable, the workflow must skip with an
  explicit reason instead of failing as a regression.
- If a fixture omits required metadata, the workflow must fail as a harness
  error.
- If both tools succeed but no comparable output artifact is found, the
  workflow must fail as a harness error.
- If one tool succeeds and the other fails for a `success` fixture, the
  workflow must fail.
- If one tool fails and the other succeeds for an `error` fixture, the
  workflow must fail.
- If a documented divergence fixture unexpectedly matches fully, the workflow
  may report that as a candidate for reclassification.
- Family-specific syntax accepted by `vasm` but intentionally rejected by
  opForge must not be placed in the shared-parity corpus unless marked as a
  documented divergence.
- opForge-only directives and macros must stay out of the shared-parity corpus.

## Decision Criteria
- [ ] `DC-EXTAB-001`: The repository contains a dedicated external-oracle corpus
      separate from `examples/reference`.
- [ ] `DC-EXTAB-002`: A `vasm`-backed 680x0 adapter can run a curated corpus of
      success fixtures and compare normalized flat bytes.
- [ ] `DC-EXTAB-003`: The same workflow can run a curated corpus of negative
      fixtures and compare normalized error classes.
- [ ] `DC-EXTAB-004`: The workflow is environment-gated and skips cleanly when
      `vasm` is unavailable.
- [ ] `DC-EXTAB-005`: Mismatch reports are structured enough to identify the
      fixture, CPU, oracle, and concrete difference without manually rerunning
      both tools first.
- [ ] `DC-EXTAB-006`: The fixture metadata and adapter boundary are generic
      enough to support at least one non-680x0 family in a future follow-on
      plan without redesigning the entire harness.
- [ ] `DC-EXTAB-007`: The first shipped slice can exercise at least a focused
      `68000`/`68010` shared subset without touching the normal example
      reference workflow.

## Proposed Validation
- Add fixture-schema validation tests.
- Add adapter availability tests that skip cleanly when the oracle executable
  is not configured.
- Add positive shared-subset fixtures for `68000` and `68010`.
- Add negative shared-subset fixtures for `68000` and `68010`.
- Add at least one documented-divergence fixture proving the divergence path is
  visible but non-failing.
- Verify that running the normal example/reference workflow is unaffected by the
  external-oracle workflow being absent.
- Verify that the external-oracle workflow can run in a dedicated CI job with
  explicit tool installation or preinstalled tooling.
- Keep the active worktree `AGENTS.md` workflow and execution rules binding at
  all times for any follow-on plan or implementation work derived from this
  proposal.

## Open Questions
- Should the fixture metadata live as one sidecar file per fixture or as a
  family-level manifest that indexes many fixtures?
- Should documented divergences fail if the divergence changes shape, or only if
  the fixture becomes fully comparable and still mismatches?
- Should the first `vasm` adapter support only flat binary output, or should it
  define a reserved but unused object-output path from the start?
- Should the first implementation live in Rust test code, a reusable library
  helper, or a small CLI under `xtask`-style tooling?
