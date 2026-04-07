# opForge External-Oracle A/B Improvement Spec (v0.1)

## Summary
This specification proposes a structural and behavioral improvement to the
external-oracle A/B testing workflow.

The two core changes are:

- keep fixture source files in a neutral, stable source corpus rather than
  physically classifying source files into `positive`, `negative`, and
  `documented_divergence` directories
- reduce syntax-driven false differences (especially in the `mos6502` family)
  by using shared-subset neutral fixture notation and adapter/harness-side
  normalization where safe and deterministic

The classification categories remain part of workflow outputs, reporting, and
review summaries, but they should be derived metadata and report artifacts,
not the primary source-tree organization of fixture code.

This spec also requires a follow-on normalized full specification after
implementation lands, so the new model becomes the canonical contract rather
than an incremental proposal.

## Problem
The current corpus model relies on source-file placement inside classified
folders (`positive`, `negative`, `documented_divergence`). This creates three
practical issues.

First, source intent and execution outcome classification are conflated. A
fixture's authored source and its expected/outcome semantics become coupled to
directory placement, which increases churn when behavior is reclassified.

Second, classification-based folder moves make review history noisier than
necessary. The same source may move between categories across tool maturity
without meaningful source-level change, obscuring real semantic deltas.

Third, family-specific syntax variance (notably across `mos6502` and `64tass`
interactions) still introduces avoidable report noise. Some mismatches are
real compatibility gaps, but others are notation-level differences that could
be normalized in fixture style or adapter behavior.

## Goals
- [ ] `REQ-EXTAB-IMPR-001`: Separate fixture source storage from classification
      and reporting storage.
- [ ] `REQ-EXTAB-IMPR-002`: Keep source fixtures under neutral corpus roots,
      with classification represented in manifest metadata and generated
      reports.
- [ ] `REQ-EXTAB-IMPR-003`: Preserve existing external-oracle categories
      (`success`, `error`, `documented_divergence`) as logical outcomes, but do
      not require source-file moves to represent reclassification.
- [ ] `REQ-EXTAB-IMPR-004`: Reduce syntax-only mismatch noise by defining and
      enforcing a family-specific shared-neutral notation policy for fixture
      authoring.
- [ ] `REQ-EXTAB-IMPR-005`: Improve oracle adapter normalization hooks so
      non-semantic syntax differences are handled deterministically where safe.
- [ ] `REQ-EXTAB-IMPR-006`: Maintain opt-in, environment-gated execution and
      keep default local/CI paths dependency-free.
- [ ] `REQ-EXTAB-IMPR-007`: Keep mismatch reporting actionable and explicitly
      distinguish semantic mismatches from normalized notation differences.
- [ ] `REQ-EXTAB-IMPR-008`: Require a follow-on normalized full specification
      after implementation, capturing the final canonical corpus and reporting
      contract.

## Non-Goals
- [ ] `NREQ-EXTAB-IMPR-001`: Remove `success`/`error`/
      `documented_divergence` as workflow concepts.
- [ ] `NREQ-EXTAB-IMPR-002`: Hide real behavioral differences behind overly
      permissive normalization.
- [ ] `NREQ-EXTAB-IMPR-003`: Collapse external-oracle corpus into
      `examples/reference`.
- [ ] `NREQ-EXTAB-IMPR-004`: Introduce non-deterministic rewrite stages that
      change fixture semantics.
- [ ] `NREQ-EXTAB-IMPR-005`: Require external tool installation for default
      workspace tests.

## Invariants / Constraints
- The active worktree `AGENTS.md` workflow and execution rules remain binding
  for any follow-on plan or implementation derived from this spec.
- External-oracle source corpus remains separate from `examples/reference`.
- Classification remains explicit and visible in manifests and reports.
- Any normalization step must be deterministic, auditable, and semantics-safe.
- If normalization cannot be proven semantics-safe for a fixture family, the
  fixture must remain explicit and unnormalized.
- `documented_divergence` remains a first-class expected outcome and must not
  degrade into ad hoc skip behavior.

## Behavioral Contract

### 1. Source corpus and reporting separation
The workflow should adopt a source-first layout where fixture source files are
stored independently from classification buckets.

Recommended shape:

- `examples/ab/<family>/<oracle>/sources/...` for source fixture files
- `examples/ab/<family>/<oracle>/manifests/...` for manifest definitions
- `examples/ab/<family>/<oracle>/reports/...` for generated classified summaries

Classification (`success`, `error`, `documented_divergence`) must be declared
in metadata (`expected_outcome`) and reflected in reports, not inferred solely
from directory names.

Reclassification should modify manifest metadata and report output, not move
source fixtures unless content-level restructuring is required.

### 2. Manifest responsibilities
Manifests must become the single source of truth for:

- fixture identity and source path
- CPU/profile/oracle target
- expected outcome category
- compare mode
- divergence contract fields when outcome is `documented_divergence`
- optional normalization profile/hints

This keeps the directory tree stable while allowing outcome evolution over
time.

### 3. Reporting responsibilities
Report outputs should be generated as classified views, for example:

- summary tables grouped by `success`, `error`, and `documented_divergence`
- per-fixture mismatch detail with semantic-vs-notation classification tags
- reclassification-candidate section when observed behavior no longer matches
  expected category

Reports may be persisted under `reports/` for review workflows, but should not
be treated as editable source fixtures.

### 4. Syntax-neutrality improvement policy
For each family/oracle pair, define a shared-neutral notation profile that
prioritizes source forms both tools can parse without semantic ambiguity.

For `mos6502`/`64tass` specifically:

- prefer neutral operand and immediate notation common to both toolchains
- avoid tool-specific convenience directives in shared-subset fixtures unless
  fixture outcome is explicitly `documented_divergence`
- prefer explicit width/state signaling in `65816` fixtures where ambiguity can
  cause parser-level divergence

When neutral source notation is insufficient, adapters may apply
family-specific, semantics-preserving normalization before invocation. Such
normalization must be:

- deterministic
- reversible or at least audit-traceable in logs
- limited to documented rule sets
- covered by tests proving no semantic byte drift for normalized success cases

### 5. Mismatch taxonomy refinement
Mismatch reporting must distinguish at least:

- semantic output mismatch
- normalized error-class mismatch
- notation-normalized equivalence (source-level difference, same semantic
  result)
- adapter-normalization limitation (normalization deliberately not applied)

This allows teams to focus fixes on true compatibility gaps.

## Proposed Implementation Direction
1. Add neutral source roots and manifest indirection without removing current
   category folders immediately.
2. Introduce report-generation paths that produce category views from manifest
   metadata and run results.
3. Migrate a representative subset (starting with `mos6502`) to source-root
   references while preserving existing outcomes.
4. Add/expand notation-normalization rules in fixtures first, then in adapters
   only where fixture-level neutrality is insufficient.
5. Remove legacy source-classification folder dependence after parity and
   reporting equivalence are proven.

## Validation Criteria
- [ ] `DC-EXTAB-IMPR-001`: Fixtures can be reclassified by metadata/report
      updates without source-file moves.
- [ ] `DC-EXTAB-IMPR-002`: Existing category summaries remain available via
      generated reports.
- [ ] `DC-EXTAB-IMPR-003`: `mos6502` syntax-driven false differences are
      measurably reduced after neutral-notation and adapter improvements.
- [ ] `DC-EXTAB-IMPR-004`: True semantic mismatches remain visible and are not
      masked by normalization.
- [ ] `DC-EXTAB-IMPR-005`: Default test paths remain dependency-free; opt-in
      external-oracle execution remains skip-safe when tools are unavailable.
- [ ] `DC-EXTAB-IMPR-006`: Report output explicitly tags mismatch cause classes
      and reclassification candidates.

## Required Follow-On Artifact
After implementation completion, a normalized full specification must be
written and committed.

Required follow-on deliverable:

- a canonical post-implementation spec that supersedes this v0.1 improvement
  proposal and captures:
  - final corpus directory contract
  - final manifest schema and required fields
  - final normalization rules per family/oracle
  - final reporting taxonomy and artifact structure
  - migration guidance from legacy classification-folder layouts

This follow-on spec is mandatory before declaring the improvement program
complete.

## Open Questions
- Should reports be committed as artifacts under `examples/ab/.../reports/` or
  generated only in CI/temp output and attached to review artifacts?
- For notation normalization, which transformations belong in fixture authoring
  guidelines versus adapter preprocessing?
- Should normalization-rule versions be encoded in manifest metadata to ensure
  reproducibility across historical test runs?